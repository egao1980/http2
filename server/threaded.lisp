(in-package #:http2/server/threaded)

(defclass threaded-dispatcher (base-dispatcher)
  ((connections :accessor get-connections :initarg :connections))
  (:documentation
   "Specialize DO-NEW-CONNECTION to process new connections each in a separate thread. ")
  (:default-initargs :connections (make-array 10 :fill-pointer 0 :adjustable t)))

(defun track-connection (dispatcher connection)
  (with-slots (connections) dispatcher
    (let ((empty (position nil connections)))
      (if empty (setf (aref connections empty) connection)
          (vector-push-extend connection connections)))))

(defclass tls-threaded-dispatcher (tls-dispatcher-mixin threaded-dispatcher)
  ())

(defclass detached-tls-threaded-dispatcher (detached-server-mixin tls-threaded-dispatcher)
  ())

(defclass detached-threaded-dispatcher (detached-server-mixin threaded-dispatcher)
  ())
(defclass detached-single-client-dispatcher (detached-server-mixin single-client-dispatcher)
  ())

(defmethod do-new-connection (listening-socket (dispatcher threaded-dispatcher))
  (let ((socket (usocket:socket-accept listening-socket
                                       :element-type '(unsigned-byte 8)))
        (context cl+ssl::*ssl-global-context*)
        (connection (make-connection-object dispatcher)))
    (track-connection dispatcher connection)
    (bt:make-thread
     (lambda ()
       (cl+ssl:with-global-context (context)
         (unwind-protect
              (with-standard-handlers ()
                (restart-case
                    (with-open-stream (stream (server-socket-stream socket dispatcher))
                      (http2/server::process-server-stream stream
                                                           :connection connection))
                  (kill-client-connection () nil)))
           (setf (aref (get-connections dispatcher) (position connection (get-connections dispatcher)))
                 nil)))) ; FIXME:
     ;; TODO: peer IP and port to name?
     :name "HTTP2 server thread for connection" )))

(defclass threaded-server-mixin ()
  ((scheduler :accessor get-scheduler :initarg :scheduler)
   (lock      :accessor get-lock      :initarg :lock))
  (:default-initargs
   :scheduler *scheduler*
   :lock (bt:make-lock))
  (:documentation
   "A mixin for a connection that holds a lock in actions that write to the output network
stream, and provides a second thread for scheduled activities (e.g., periodical
events)."))

(defmethod queue-frame :around ((server threaded-server-mixin) frame)
  (bt:with-lock-held ((get-lock server))
    (call-next-method)))

(defmethod cleanup-connection :after ((connection threaded-server-mixin) &optional error)
    (stop-scheduler-in-thread (get-scheduler connection)))

(defmethod http2/server::server-socket-stream (socket (dispatcher tls-dispatcher-mixin))
  "The cl-ssl server socket."
  (with-slots (certificate-file private-key-file) dispatcher
    (cl+ssl:make-ssl-server-stream
     (call-next-method)
     :certificate certificate-file
     :key private-key-file)))

;;; ALPN select without http2/openssl grovel. SSL_TLSEXT_ERR_* are stable.
(defconstant +ssl-tlsext-err-ok+ 0)
(defconstant +ssl-tlsext-err-alert-fatal+ 2)

(cffi:defcfun ("SSL_CTX_set_alpn_select_cb" %ssl-ctx-set-alpn-select-cb) :void
  (ctx :pointer)
  (cb :pointer)
  (arg :pointer))

(cffi:defcallback select-h2-alpn :int
    ((ssl :pointer)
     (out :pointer)
     (outlen :pointer)
     (in :pointer)
     (inlen :unsigned-int)
     (arg :pointer))
  "Select h2 from the client's ALPN list; fatal alert if it was not offered."
  (declare (ignore ssl arg))
  (loop for idx = 0 then (+ idx 1 len)
        while (< idx inlen)
        for len = (logand #xff (cffi:mem-ref in :unsigned-char idx))
        when (and (= len 2)
                  (<= (+ idx 3) inlen)
                  (= (cffi:mem-ref in :unsigned-char (+ 1 idx)) (char-code #\h))
                  (= (cffi:mem-ref in :unsigned-char (+ 2 idx)) (char-code #\2)))
          do (setf (cffi:mem-ref outlen :unsigned-char) 2
                   (cffi:mem-ref out :pointer) (cffi:inc-pointer in (1+ idx)))
             (return +ssl-tlsext-err-ok+)
        finally (return +ssl-tlsext-err-alert-fatal+)))

(defun make-cl+ssl-h2-context (dispatcher)
  "SSL_CTX via cl+ssl with ALPN h2. Used by the threaded server (no grovel)."
  (declare (ignore dispatcher))
  (let ((ctx (cl+ssl:make-context
              :verify-mode cl+ssl:+ssl-verify-none+
              :verify-location nil
              :min-proto-version #x0303))) ; TLS1_2_VERSION
    (%ssl-ctx-set-alpn-select-cb ctx (cffi:callback select-h2-alpn) (cffi:null-pointer))
    ctx))

"For a TLS server wrap the global context."
(defmethod http2/server::start-server-on-socket ((server tls-threaded-dispatcher) socket)
  (cl+ssl:ensure-initialized)
  (cl+ssl:with-global-context ((make-cl+ssl-h2-context server) :auto-free-p t)
    (call-next-method)))

(define-condition not-http2-stream (serious-condition)
  ((tls-stream :accessor get-tls-stream :initarg :tls-stream)
   (alpn       :accessor get-alpn       :initarg :alpn))
  (:documentation
   "Signalled to decline handling of TLS stream as HTTP2 stream due to different ALPN.")
  (:report (lambda (condition stream)
             (format stream "The TLS stream ~A is not a HTTP2 stream (ALPN ~s)"
                     (get-tls-stream condition)
                     (get-alpn condition)))))

(defmethod get-peer-name ((object cl+ssl::ssl-stream))
  (get-peer-name (cl+ssl::ssl-stream-socket object)))

#+sbcl
(defmethod get-peer-name ((object sb-bsd-sockets:socket))
  (call-next-method)
;  (sb-bsd-sockets::socket-peerstring object)
  )

#+sbcl
(defmethod get-peer-name ((object sb-sys:fd-stream))
  (format nil "fd:~d" (sb-sys:fd-stream-fd object)))

(defmethod get-peer-name ((object stream-based-connection-mixin))
  (get-peer-name (get-network-stream object)))
