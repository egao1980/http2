(in-package http2/tests/frames)

;;;; RFC 9113 flow-control and concurrency rules, on an in-process client/server pair. These
;;;; were found by replaying model-based traces (egao1980/http2-spec) against the library.

(defclass pair-client (client-http2-connection write-buffer-connection-mixin) ())
(defclass pair-server (server-http2-connection write-buffer-connection-mixin) ())

(defun take-octets (connection)
  (let* ((buffer (get-to-write connection))
         (octets (coerce buffer '(simple-array (unsigned-byte 8) (*)))))
    (setf (fill-pointer buffer) 0)
    octets))

(defun pump (from to)
  "Deliver FROM's queued frames to TO."
  (let ((octets (take-octets from)))
    (when (plusp (length octets))
      (process-frames to octets))))

(defun make-pair (&rest server-args)
  "Client and server after the connection preface and the SETTINGS exchange."
  (let* ((client (make-instance 'pair-client))
         (server (apply #'make-instance 'pair-server server-args))
         (octets (take-octets client))
         (preface (length http2/core::+client-preface-start+)))
    (http2/core::parse-client-preface server (subseq octets 0 preface))
    (process-frames server (subseq octets preface))
    (pump server client)
    (pump client server)
    (values client server)))

(defun open-request (client)
  (let ((stream (create-new-local-stream client)))
    (send-headers stream '((:method "GET") (:scheme "https") (:path "/") (:authority "test"))
                  :end-stream nil)
    stream))

(defun frame-types (octets)
  "Types and stream ids of the frames in OCTETS, plus the error code of each RST_STREAM."
  (loop with start = 0
        while (< start (length octets))
        collect (let ((len (aref/wide octets start 3))
                      (type (aref octets (+ start 3)))
                      (id (aref/wide octets (+ start 5) 4)))
                  (prog1 (if (= type 3)
                             (list type id (aref/wide octets (+ start 9) 4))
                             (list type id))
                    (incf start (+ 9 len))))))

(fiasco:deftest settings-frame-at-nonzero-offset/test ()
  "Several frames in one buffer: a SETTINGS payload need not start at offset 0."
  (multiple-value-bind (client server) (make-pair)
    (write-settings-frame client '((:initial-window-size . 1000)))
    (write-settings-frame client '((:max-frame-size . 20000)))
    (pump client server)
    (fiasco:is (= 1000 (http2/core::get-initial-peer-window-size server)))
    (fiasco:is (= 20000 (get-max-peer-frame-size server)))))

(fiasco:deftest initial-window-size-adjusts-streams/test ()
  "§6.9.2: a SETTINGS_INITIAL_WINDOW_SIZE change applies its delta to open streams."
  (multiple-value-bind (client server) (make-pair)
    (let ((stream (open-request client)))
      (pump client server)
      (write-settings-frame server '((:initial-window-size . 100)))
      (pump server client)
      (fiasco:is (= 100 (get-peer-window-size stream)))
      (write-settings-frame server '((:initial-window-size . 200)))
      (pump server client)
      (fiasco:is (= 200 (get-peer-window-size stream))))))

(fiasco:deftest initial-window-size-overflow/test ()
  "§6.9.2: a change pushing a stream window past 2^31-1 is a connection FLOW_CONTROL_ERROR."
  (multiple-value-bind (client server) (make-pair)
    (let ((stream (open-request client)))
      (pump client server)
      (write-window-update-frame (find (http2/core::get-stream-id stream) (get-streams server)
                                       :key #'http2/core::get-stream-id)
                                 (- +max-window-size+ 65535))
      (pump server client)
      (write-settings-frame server '((:initial-window-size . 65536)))
      (fiasco:signals http2/core::window-overflow (pump server client)))))

(fiasco:deftest initial-window-size-too-large/test ()
  "§6.5.2: SETTINGS_INITIAL_WINDOW_SIZE above 2^31-1 is a FLOW_CONTROL_ERROR."
  (multiple-value-bind (client server) (make-pair)
    (write-settings-frame server `((:initial-window-size . ,(1+ +max-window-size+))))
    (handler-case (progn (pump server client) (fiasco:is nil))
      (connection-error (e)
        (fiasco:is (= http2/core::+flow-control-error+ (http2/core::get-code e)))))))

(fiasco:deftest window-update-overflow/test ()
  "§6.9.1: WINDOW_UPDATE past 2^31-1 is a connection error on stream 0, else a stream error."
  (multiple-value-bind (client server) (make-pair)
    (write-window-update-frame server +max-window-size+)
    (fiasco:signals http2/core::window-overflow (pump server client)))
  (multiple-value-bind (client server) (make-pair)
    (let ((stream (open-request client)))
      (pump client server)
      (write-window-update-frame (find (http2/core::get-stream-id stream) (get-streams server)
                                       :key #'http2/core::get-stream-id)
                                 +max-window-size+)
      (fiasco:signals http2/core::stream-window-overflow (pump server client)))))

(fiasco:deftest push-promise-is-a-protocol-error/test ()
  "§8.4: a client that disabled push answers PUSH_PROMISE with a connection PROTOCOL_ERROR."
  (multiple-value-bind (client server) (make-pair)
    (let ((stream (open-request client)))
      (pump client server)
      (write-push-promise-frame (find (http2/core::get-stream-id stream) (get-streams server)
                                      :key #'http2/core::get-stream-id)
                                '() 2)
      (fiasco:signals http2/core::push-promise-refused (pump server client)))))

(fiasco:deftest max-concurrent-streams/test ()
  "§5.1.2: streams above our SETTINGS_MAX_CONCURRENT_STREAMS are refused; the peer learns the limit."
  (multiple-value-bind (client server) (make-pair :max-concurrent-streams 1)
    (fiasco:is (eql 1 (get-peer-max-concurrent-streams client)))
    (open-request client)
    (pump client server)
    (open-request client)
    (pump client server)
    (fiasco:is (equal '((3 3 #.http2/core::+refused-stream+)) (frame-types (take-octets server))))
    (fiasco:is (equal '(1) (mapcar #'http2/core::get-stream-id (get-streams server))))
    (open-request client)
    (pump client server)
    (fiasco:is (equal '((3 5 #.http2/core::+refused-stream+)) (frame-types (take-octets server)))
               "a refused header block still updates HPACK state")))

(defclass window-collecting-stream (server-stream body-collecting-mixin) ()
  (:documentation "Server stream that credits consumed DATA, as grpc-http2-stream does."))

(defclass gzip-window-stream (server-stream header-collecting-mixin gzip-decoding-mixin) ()
  (:documentation "Server stream whose only WINDOW_UPDATE comes from the gzip mixin.

SERVER-STREAM comes first so its ADD-HEADER method records the pseudo-headers.
HEADER-COLLECTING-MIXIN is only here so the gzip mixin can call GET-HEADERS."))

(defun window-update-increments (octets)
  "31-bit increments of WINDOW_UPDATE frames (type 8) in OCTETS."
  (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (loop with start = 0
          while (< start (length octets))
          for len = (aref/wide octets start 3)
          for type = (aref octets (+ start 3))
          when (= type 8)
            collect (ldb (byte 31 0) (aref/wide octets (+ start 9) 4))
          do (incf start (+ 9 len)))))

(fiasco:deftest empty-data-omits-zero-window-update/test ()
  "§6.9: an empty DATA frame consumes no window, so the receiver must not send WINDOW_UPDATE 0.

gRPC half-closes with an empty DATA + END_STREAM. Increment 0 on stream 0 is a
connection PROTOCOL_ERROR; grpcio and this library both reject it."
  (multiple-value-bind (client server)
      (make-pair :stream-class 'window-collecting-stream)
    (let ((stream (open-request client)))
      (pump client server)
      (write-data-frame stream (make-octet-buffer 0) :end-stream t)
      (pump client server)
      (fiasco:is (null (window-update-increments (get-to-write server))))
      (pump server client)
      (fiasco:is (= 65535 (http2/core::get-window-size server)))
      (fiasco:is (= 65535 (get-peer-window-size client)))))
  (multiple-value-bind (client server)
      (make-pair :stream-class 'window-collecting-stream)
    (let ((stream (open-request client)))
      (pump client server)
      (write-data-frame stream (make-initialized-octet-buffer #(1 2 3)) :end-stream t)
      (pump client server)
      (fiasco:is (equal '(3 3) (window-update-increments (get-to-write server))))
      (pump server client)
      (fiasco:is (= 65535 (get-peer-window-size client)))
      (fiasco:is (= 65535 (get-peer-window-size stream)))))
  (multiple-value-bind (client server)
      (make-pair :stream-class 'gzip-window-stream)
    (let ((stream (open-request client)))
      (pump client server)
      (handler-bind ((warning #'muffle-warning))
        (write-data-frame stream (make-octet-buffer 0) :end-stream t)
        (pump client server))
      (fiasco:is (null (window-update-increments (get-to-write server))))
      (pump server client))))
