;;;; QL mgl-pax is findable, so canned extra-with never installs the OCI macros
;;;; package (collect-missing treats extras as already present). The QL dist also
;;;; ships autoload/ that is not AUTOLOAD:AUTOLOAD-SYSTEM. Evict those trees,
;;;; then pin OCI autoload / dref / mgl-pax (not mgl-pax-bootstrap).

(defun %ql-software-roots ()
  (let ((homes (remove-duplicates
                (remove nil (list (uiop:getenv "HOME")
                                  (uiop:getenv "USERPROFILE")
                                  (ignore-errors (namestring (user-homedir-pathname)))
                                  "/root"
                                  "/github/home"))
                :test #'string=)))
    (loop for home in homes
          nconc (loop for rel in '(".roswell/lisp/quicklisp/dists/quicklisp/software/"
                                   "quicklisp/dists/quicklisp/software/")
                      for dir = (merge-pathnames rel (uiop:ensure-directory-pathname home))
                      when (probe-file dir)
                        collect dir))))

(defun %prefix-dir-p (dir-name prefix)
  (let ((n (string-downcase dir-name))
        (p (string-downcase prefix)))
    (or (string= n p)
        (and (> (length n) (length p))
             (string= n p :end1 (length p))
             (char= (char n (length p)) #\-)))))

(defun %evict-ql-prefix (prefix)
  (dolist (software (%ql-software-roots))
    (dolist (dir (uiop:subdirectories (uiop:ensure-directory-pathname software)))
      (let ((name (car (last (pathname-directory dir)))))
        (when (and name (%prefix-dir-p name prefix))
          (format t "~&; ci: evict QL dummy ~a~%" dir)
          (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir)
                                      :validate (constantly t)
                                      :if-does-not-exist :ignore))))))

(defun %ql-source-p (system-name)
  (let ((src (ignore-errors (namestring (asdf:system-source-directory system-name)))))
    (and src (search "quicklisp" src :test #'char-equal))))

(format t "~&; ci: evict QL mgl-pax/dref dummy so OCI macros win~%")
(%evict-ql-prefix "mgl-pax")
(%evict-ql-prefix "dref")

(dolist (name '("mgl-pax" "mgl-pax-bootstrap" "dref" "autoload"))
  (ignore-errors (asdf:clear-system name)))

(let ((ensure (find-symbol "ENSURE-SYSTEMS" :cl-repo)))
  (unless ensure
    (error "cl-repo:ensure-systems missing after client load"))
  (funcall ensure '("autoload" "dref" "mgl-pax") :force t :default-source :oci))

(dolist (name '("autoload" "dref" "mgl-pax"))
  (ignore-errors (asdf:clear-system name))
  (let ((sys (asdf:find-system name nil)))
    (unless sys
      (error "OCI ~a not findable after pre-install" name))
    (when (%ql-source-p name)
      (error "~a still resolving to Quicklisp dummy: ~a"
             name (asdf:system-source-directory sys)))
    (format t "~&; ci: ~a from ~a~%" name (asdf:system-source-directory sys))))

(asdf:load-system "autoload")
(unless (and (find-package "AUTOLOAD")
             (find-symbol "AUTOLOAD-SYSTEM" "AUTOLOAD"))
  (error "loaded autoload from ~a but AUTOLOAD:AUTOLOAD-SYSTEM is missing"
         (asdf:system-source-directory "autoload")))
