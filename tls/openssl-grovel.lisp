(in-package #:http2/openssl)

;;; Portable include/lib flags. Upstream hardcoded Homebrew @3, which breaks
;;; Linux/Windows grovel and any Darwin install that is not that prefix.
#.(flet ((split-flags (s)
           (remove "" (uiop:split-string s :separator " ") :test #'string=))
         (include-if (dir)
           (let* ((root (uiop:ensure-directory-pathname dir))
                  (hdr (merge-pathnames "openssl/ssl.h" root)))
             (when (probe-file hdr)
               (format nil "-I~a" (namestring root))))))
    (let* ((env-c (uiop:getenv "OPENSSL_CFLAGS"))
           (env-l (uiop:getenv "OPENSSL_LIBS"))
           (pkg-c (ignore-errors
                    (string-trim '(#\Space #\Newline #\Tab)
                                 (uiop:run-program '("pkg-config" "--cflags" "openssl")
                                                   :output :string
                                                   :ignore-error-status t))))
           (pkg-l (ignore-errors
                    (string-trim '(#\Space #\Newline #\Tab)
                                 (uiop:run-program '("pkg-config" "--libs" "openssl")
                                                   :output :string
                                                   :ignore-error-status t))))
           (probed (remove nil
                           (mapcar #'include-if
                                   '("/opt/homebrew/opt/openssl@3/include"
                                     "/opt/homebrew/opt/openssl/include"
                                     "/usr/local/opt/openssl@3/include"
                                     "/usr/local/opt/openssl/include"
                                     "/opt/local/include"
                                     "/usr/local/include"
                                     "/usr/include"))))
           (lib-dirs (remove nil
                             (mapcar (lambda (dir)
                                       (when (probe-file dir)
                                         (format nil "-L~a" dir)))
                                     '("/opt/homebrew/opt/openssl@3/lib"
                                       "/opt/homebrew/opt/openssl/lib"
                                       "/usr/local/opt/openssl@3/lib"
                                       "/usr/local/opt/openssl/lib"
                                       "/opt/local/lib"
                                       "/usr/local/lib"))))
           (cflags (append (when (and env-c (plusp (length env-c))) (split-flags env-c))
                           (when (and pkg-c (plusp (length pkg-c))
                                      (not (search "not found" pkg-c)))
                             (split-flags pkg-c))
                           probed))
           (lflags (append (when (and env-l (plusp (length env-l))) (split-flags env-l))
                           (when (and pkg-l (plusp (length pkg-l))
                                      (not (search "not found" pkg-l)))
                             (split-flags pkg-l))
                           lib-dirs)))
      `(progn
         ,@(when cflags `((cc-flags ,@cflags)))
         ,@(when lflags `((cc-flags ,@lflags))))))

(include "openssl/ssl.h")
(constant (ssl-error-none "SSL_ERROR_NONE"))
(constant (ssl-error-want-write "SSL_ERROR_WANT_WRITE"))
(constant (ssl-error-want-read "SSL_ERROR_WANT_READ"))
(constant (ssl-error-ssl "SSL_ERROR_SSL"))
(constant (ssl-error-syscall "SSL_ERROR_SYSCALL"))
(constant (ssl-error-zero-return "SSL_ERROR_ZERO_RETURN"))

(constant (ssl-tlsext-err-ok "SSL_TLSEXT_ERR_OK"))
(constant (ssl-tlsext-err-alert-fatal "SSL_TLSEXT_ERR_ALERT_FATAL"))
(constant (ssl-tlsext-err-noack "SSL_TLSEXT_ERR_NOACK"))

(constant (ssl-filetype-pem "SSL_FILETYPE_PEM"))
(constant (tls-1.2-version "TLS1_2_VERSION"))
(constant (ssl-op-all "SSL_OP_ALL"))

(constant (ssl-ctrl-set-min-proto-version "SSL_CTRL_SET_MIN_PROTO_VERSION"))

(constant (bio-flags-should-retry "BIO_FLAGS_SHOULD_RETRY"))
