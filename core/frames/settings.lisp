(in-package http2/core)

(defgeneric peer-expects-settings-ack (connection)
  (:documentation
   "Called when settings-frame without ACK is received, after individual
SET-PEER-SETTING calls. By default, send ACK frame.")

  (:method (connection)
    (write-ack-setting-frame connection)))

(defgeneric peer-acks-settings (connection)
  (:documentation
   "Called when SETTINGS-FRAME with ACK flag is received. By default does nothing.")
  (:method (connection)))

(defgeneric set-peer-setting (connection name value)
  (:documentation
   "Process received information about peers setting.

The setting relates to the CONNECTION. NAME is a keyword symbol (see
*SETTINGS*, subject to possible change to 16bit number in future) and VALUE is
32bit number.")
  (:method (connection name value)
    "Fallback."
    (declare (type (unsigned-byte 32) value))
    (warn 'unimplemented-feature :format-control "Peer settings not used - ~a ~a."
                                 :format-arguments (list name value)))

  (:method (connection (name (eql :header-table-size)) value)
    (declare (type (unsigned-byte 32) value))
    (let ((context (get-decompression-context connection)))
      (when (< value (the (unsigned-byte 32) (get-dynamic-table-size context)))
        (update-dynamic-table-size context value))
      (push value (get-updates-needed context))))

  (:method (connection (name (eql :initial-window-size)) value)
    (declare (type (unsigned-byte 32) value))
    (when (> value +max-window-size+)
      (connection-error 'incorrect-initial-window-size-value connection
                        :value value))
    ;; RFC 9113 §6.9.2: the change adjusts the send window of every stream we may still
    ;; send on, and a window it pushes above 2^31-1 is a connection FLOW_CONTROL_ERROR.
    (let ((delta (- value (get-initial-peer-window-size connection)))
          (adjusted (remove-if-not (lambda (s)
                                     (member (get-state s) '(open half-closed/remote reserved/local)))
                                   (get-streams connection))))
      (when (some (lambda (s) (> (+ (get-peer-window-size s) delta) +max-window-size+)) adjusted)
        (connection-error 'window-overflow connection :increment delta))
      (dolist (s adjusted)
        (incf (get-peer-window-size s) delta))
      (setf (get-initial-peer-window-size connection) value)
      (when (plusp delta)
        (dolist (s adjusted)
          (let ((callback (get-window-size-increment-callback s)))
            (when callback
              (continue-long-write s callback (get-max-peer-frame-size connection))))))))

  (:method (connection (name (eql :max-frame-size)) value)
    (if (>= 16777215 value 16384)
        (setf (get-max-peer-frame-size connection) value)
        (connection-error 'incorrect-frame-size-value
                          :value value)))

  (:method (connection (name (eql :no-rfc5740-priorities)) value)
    ;; we must signal if not 0 or 1. We MAY if it changes afterwards, so we do
    ;; not. (rfc9218)
    (unless (typep value 'bit)
      (connection-error 'incorrect-setting-value connection
                        :setting :no-rfc5740-priorities
                        :allowed 'bit
                        :value value)))

  (:method (connection (name (eql :max-header-list-size)) value)
    (setf (get-peer-max-headers-size connection) value))

  (:method ((connection client-http2-connection) (name (eql :enable-push)) value)
    (declare (type (unsigned-byte 32) value))
    (unless (zerop value)
      (connection-error 'incorrect-enable-push-value connection
                        :value value)))

  (:method ((connection server-http2-connection) (name (eql :enable-push)) value)
    (declare (type (unsigned-byte 32) value))
    (if (> value 1)
        (connection-error 'incorrect-enable-push-value connection)
        (setf (get-peer-accepts-push connection) (plusp value))))

  (:method (connection (name (eql :max-concurrent-streams)) value)
    (setf (get-peer-max-concurrent-streams connection) value)))

(define-frame-type 4 :settings-frame
    "```
    +-------------------------------+
    |       Identifier (16)         |
    +-------------------------------+-------------------------------+
    |                        Value (32)                             |
    +---------------------------------------------------------------+
   ```

   The SETTINGS frame (type=0x4) conveys configuration parameters that
   affect how endpoints communicate, such as preferences and constraints
   on peer behavior.  The SETTINGS frame is also used to acknowledge the
   receipt of those parameters.  Individually, a SETTINGS parameter can
   also be referred to as a \"setting\"."
    ((settings list))
    (:length (* (length settings) 6)
     :must-have-connection t
     :flags (ack))
    ;; writer
    nil

    ;;reader
    (lambda (connection data http-stream flags)

      "Parse settings frame. If this is ACK settings frame, invoke PEER-ACKS-SETTINGS
callback, otherwise invoke SET-PEER-SETTING callback for each setting in the
recieved order."
      ;; LENGTH is the end index of the payload in DATA, which need not start at 0
      ;; (PROCESS-FRAMES passes whole buffers).
      (cond
        ((get-flag flags :ack)
         (when (> length start)
           (error "Ack settings frame must be empty. We should close connection."))
         (peer-acks-settings connection))
        (t
         (unless (zerop (mod (- length start) 6))
           (connection-error 'incorrect-settings-frame-size connection))
         (loop
           for idx from start below length by 6
           for identifier = (aref/wide data idx 2)
           and value = (aref/wide data (+ 2 idx) 4)
           for name = (find-setting-by-id identifier)
           ;;    An endpoint that receives a SETTINGS frame with any unknown or
           ;;    unsupported identifier MUST ignore that setting.
           when name
             do (set-peer-setting connection name value)
           finally (peer-expects-settings-ack connection))))
      (values #'parse-frame-header 9)))

(defun write-settings-frame (connection settings &rest keys)
   "```
     +-------------------------------+
     |       Identifier (16)         |
     +-------------------------------+-------------------------------+
     |                        Value (32)                             |
     +---------------------------------------------------------------+
    ```

    The SETTINGS frame (type=0x4) conveys configuration parameters that
    affect how endpoints communicate, such as preferences and constraints
    on peer behavior.  The SETTINGS frame is also used to acknowledge the
    receipt of those parameters.  Individually, a SETTINGS parameter can
    also be referred to as a \"setting\"."
   (let ((length (* (length settings) 6)))
     (write-frame connection length +settings-frame+ keys
                  (lambda (buffer start settings)
                    (loop for i from start by 6
                          and setting in settings
                          do (setf (aref/wide buffer i 2)
                                     (if (numberp (car setting))
                                         (car setting)
                                         (find-setting-code (car setting)))
                                   (aref/wide buffer (+ i 2) 4) (cdr setting))
                          finally (return buffer)))
                  settings)))

(defun write-ack-setting-frame (connection)
  "Write ACK settings frame.

   ACK (0x1):  When set, bit 0 indicates that this frame acknowledges
      receipt and application of the peer's SETTINGS frame.  When this
      bit is set, the payload of the SETTINGS frame MUST be empty.
      Receipt of a SETTINGS frame with the ACK flag set and a length
      field value other than 0 MUST be treated as a connection error
      (Section 5.4.1) of type FRAME_SIZE_ERROR.  For more information,
      see Section 6.5.3 (\"Settings Synchronization\")."
  (write-frame connection 0 +settings-frame+ '(:ack t) nil))
