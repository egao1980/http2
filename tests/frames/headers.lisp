(in-package http2/tests/frames)

(defvar *custom-headers-code*
  (vector-from-hex-text
   "400a637573746f6d2d6b65790d637573746f6d2d686561646572"))

(defun write-header-split (stream data splits &rest args)
  "Write headers in DATA in one header and several continuation frames to STREAM.

SPLITS sets where the data are split.
"
  (loop for (from to) on (append (list 0) splits)
        for fn = 'write-headers-frame then 'write-continuation-frame
        do
           (apply fn stream (subseq data from to) :end-headers (null to) args)))

(fiasco:deftest write-headers-frame/test ()
  (handler-bind ((http2/core::no-new-header-action 'muffle-warning))
    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         #(0 0 26 1 0 0 0 0 42 64 10 99 117 115 116 111 109 45
                           107 101 121 13 99 117 115 116 111 109 45 104 101 97
                           100 101 114)
                         *custom-headers-code*)

    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         (make-initialized-octet-buffer
                          #(0 0 26 1 4 0 0 0 42 64 10 99 117 115 116 111 109 45
                            107 101 121 13 99 117 115 116 111 109 45 104 101 97
                            100 101 114))
                         *custom-headers-code*
                         :end-headers t)

    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         #(0 0 31 1 13 0 0 0 42 4 64 10 99 117 115 116 111 109 45
                           107 101 121 13 99 117 115 116 111 109 45 104 101 97
                           100 101 114 1 2 3 4)
                         *custom-headers-code*
                         :end-headers t
                         :end-stream t
                         :padded (make-initialized-octet-buffer #(1 2 3 4)))

    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         #(0 0 36 1 45 0 0 0 42 4 0 0 0 12 27 64 10 99 117 115
                           116 111 109 45 107 101 121 13 99 117 115 116 111 109
                           45 104 101 97 100 101 114 1 2 3 4)
                         *custom-headers-code*
                         :end-headers t
                         :end-stream t
                         :padded (make-initialized-octet-buffer #(1 2 3 4))
                         :priority (make-priority :exclusive nil :stream-dependency 12
                                                  :weight 27))))

(fiasco:deftest write-priority-frame/test ()
  (test-write-parse-fn #'write-priority-frame
                       nil
                       #(0 0 5 2 0 0 0 0 42 0 0 0 12 27)
                       (make-priority :exclusive nil :stream-dependency 12
                                                :weight 27)))

(fiasco:deftest continuation-frames/test ()
  "Split the headers in several ways."
  (flet ((@ (split data)
             (test-write-parse-fn #'write-header-split
              '(:headers (("custom-key" . "custom-header")))
              data
              *custom-headers-code* split)))
    (@ nil #(0 0 26 1 4 0 0 0 42 64 10 99 117 115 116 111 109 45 107 101 121 13
             99 117 115 116 111 109 45 104 101 97 100 101 114))
    (@ '(4)
        #(0 0 4 1 0 0 0 0 42 64 10 99 117 0 0 22 9 4 0 0 0 42
          115 116 111 109 45 107 101 121 13 99 117 115 116 111
          109 45 104 101 97 100 101 114))
    (@ '(0)
        #(0 0 0 1 0 0 0 0 42 0 0 26 9 4 0 0 0 42 64 10 99 117
          115 116 111 109 45 107 101 121 13 99 117 115 116 111
          109 45 104 101 97 100 101 114))
    (@ '(0 0)
        #(0 0 0 1 0 0 0 0 42 0 0 0 9 0 0 0 0 42 0 0 26 9 4 0 0 0
          42 64 10 99 117 115 116 111 109 45 107 101 121 13 99
          117 115 116 111 109 45 104 101 97 100 101 114))))


(in-package #:http2/tests/headers)

(defsection @overview ()
  "Test handling of hpack errors and well-known bad situations in headers code.")

(defsuite
    (http2/tests/headers :bind-to-package #:http2/tests/headers
                       :in http2/tests::http2/tests))

(deftest zero-index ()
  "Test that zero-entry index gives proper error"
  (signals http2/core::decompression-failed
    (http2/tests/support:with-dummy-stream (dummy) (http2/core::read-and-add-headers #(128) dummy 0 1 0 0))))

(deftest authority ()
  "Header name from indexed without value gives an error. "
  (signals http2/core::decompression-failed
    (http2/tests/support:with-dummy-stream (dummy) (http2/core::read-and-add-headers #(129) dummy 0 1 0 0))))

(deftest bad-header ()
  "An upper-case header name is a stream error, signalled at the end of the
header block (END_HEADERS)."
  (signals http2/core::http-stream-error
    (http2/tests/support:with-dummy-stream (dummy)
      (let ((bad-header (http2/hpack::compile-headers '(("Foo" "bar")) nil)))
        (http2/core::read-and-add-headers bad-header
                                          dummy 0 (length bad-header) 4 0)))))

(deftest long-header-value ()
  "A field longer than 4 KiB decodes: www.cloudflare.com sends one at times, and a
field the decoder refuses breaks the connection's HPACK state."
  (http2/tests/support:with-dummy-stream (dummy)
    (let* ((value (make-string 5000 :initial-element #\a))
           (block (http2/hpack::compile-headers `(("link" ,value)) nil)))
      (http2/core::read-and-add-headers block dummy 0 (length block) 4 0)
      (is (equal value (cdr (assoc "link" (http2/core::get-headers dummy)
                                   :test #'equal)))))))

(deftest header-size-limit-is-per-header-block ()
  "103 Early Hints, the final response and trailers are separate header lists
(RFC 9113 §6.5.2): each gets the whole *MAX-HEADERS-SIZE*."
  (let ((http2/core::*max-headers-size* 300))
    (http2/tests/support:with-dummy-stream (dummy)
      (let ((block (http2/hpack::compile-headers
                    `(("link" ,(make-string 150 :initial-element #\l))) nil)))
        (http2/core::read-and-add-headers block dummy 0 (length block) 4 0)
        (http2/core::read-and-add-headers block dummy 0 (length block) 4 0)
        (is (= 2 (count "link" (http2/core::get-headers dummy)
                        :key #'car :test #'equal)))))))

(defun call-with-two-streams (fn)
  "Call FN with an HPACK encoder context and two dummy streams on one connection."
  (http2/tests/support:with-dummy-stream (bad)
    (funcall fn
             (make-instance 'http2/hpack::hpack-context)
             bad
             (make-instance 'http2/tests/support::dummy-stream
                            :connection (http2/core::get-connection bad)
                            :stream-id 44))))

(defun encode (headers encoder)
  "HEADERS compiled with ENCODER, as a simple octet vector (what frames carry)."
  (coerce (http2/hpack::compile-headers headers encoder)
          '(simple-array (unsigned-byte 8) (*))))

(defun read-block (block stream)
  "Read BLOCK as one HEADERS frame with END_HEADERS."
  (http2/core::read-and-add-headers block stream 0 (length block) 4 0))

(deftest stream-error-keeps-hpack-in-sync ()
  "A stream error in a header block (upper-case name) still decodes the rest of the
block, so the dynamic table keeps the entries added after the bad header and a
later block that references them decodes (RFC 9113 §4.3)."
  (call-with-two-streams
   (lambda (encoder bad good)
     (let ((block-1 (encode '(("x-before" "1") ("Bad-Name" "2") ("x-after" "3")) encoder))
           (block-2 (encode '(("x-before" "1") ("x-after" "3")) encoder)))
       ;; Indexed references only: both fields come from the dynamic table.
       (is (= 2 (length block-2)))
       (signals http2/core::lowercase-header-field-name (read-block block-1 bad))
       (read-block block-2 good)
       (is (equal '(("x-after" . "3") ("x-before" . "1"))
                  (http2/core::get-headers good)))))))

(deftest stream-error-keeps-hpack-in-sync/continuation ()
  "The bad header is in the HEADERS frame and more entries come in a CONTINUATION
frame: the error waits for END_HEADERS and the continuation is decoded."
  (call-with-two-streams
   (lambda (encoder bad good)
     (let* ((fragment-1 (encode '(("x-before" "1") ("Bad-Name" "2")) encoder))
            (fragment-2 (encode '(("x-after" "3")) encoder))
            (block-2 (encode '(("x-before" "1") ("x-after" "3")) encoder))
            (connection (http2/core::get-connection bad))
            (continuation-header
             (make-array 9 :element-type '(unsigned-byte 8)
                           :initial-contents (list 0 0 (length fragment-2) 9 4 0 0 0 42))))
       ;; No END_HEADERS: no error yet, a CONTINUATION frame header is expected.
       (let ((read-continuation-header
              (http2/core::read-and-add-headers fragment-1 bad 0 (length fragment-1) 0 0)))
         (multiple-value-bind (read-continuation size)
             (funcall read-continuation-header connection continuation-header)
           (is (= size (length fragment-2)))
           (signals http2/core::lowercase-header-field-name
             (funcall read-continuation connection fragment-2 0 (length fragment-2)))))
       (read-block block-2 good)
       (is (equal '(("x-after" . "3") ("x-before" . "1"))
                  (http2/core::get-headers good)))))))

(deftest too-large-headers-keeps-hpack-in-sync ()
  "A header list over *MAX-HEADERS-SIZE* is a stream error. The block is decoded
to its end first, so the connection stays usable."
  (let ((http2/core::*max-headers-size* 300))
    (call-with-two-streams
     (lambda (encoder bad good)
       (let ((block-1 (encode `(("x-big" ,(make-string 400 :initial-element #\b))
                                ("x-after" "3"))
                              encoder))
             (block-2 (encode '(("x-after" "3")) encoder)))
         (signals http2/core::too-large-headers (read-block block-1 bad))
         (read-block block-2 good)
         (is (equal '(("x-after" . "3")) (http2/core::get-headers good))))))))
