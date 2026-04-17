(in-package #:cl-user)

(require :asdf)

(defparameter *tests-root*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename* *default-pathname-defaults*)))

(defparameter *repo-root*
  (uiop:merge-pathnames* #p"../" *tests-root*))

(defun ensure-local-cache ()
  (unless (uiop:getenv "XDG_CACHE_HOME")
    (error "XDG_CACHE_HOME must be set by the shell wrapper")))

(defun maybe-set-max-forks ()
  (let ((value (uiop:getenv "POIU_MAX_FORKS")))
    (when value
      (let ((variable (find-symbol "*MAX-FORKS*" "POIU/FORK")))
        (unless variable
          (error "POIU/FORK:*MAX-FORKS* is not available"))
        (setf (symbol-value variable) (parse-integer value :junk-allowed t))))))

(defun ensure-output-directory-for-source (pathname)
  (ensure-directories-exist (asdf:apply-output-translations pathname)))

(defun maybe-load-quicklisp ()
  (let* ((home (user-homedir-pathname))
         (setup (merge-pathnames #p"quicklisp/setup.lisp" home))
         (source (merge-pathnames #p"quicklisp/quicklisp/package.lisp" home)))
    (when (probe-file source)
      (ensure-output-directory-for-source source))
    (when (probe-file setup)
      (load setup))))

(defun maybe-load-ocicl ()
  (let ((runtime (merge-pathnames #p".local/share/ocicl/ocicl-runtime.lisp"
                                  (user-homedir-pathname))))
    (when (probe-file runtime)
      (load runtime))))

(defun requested-system-name ()
  (or (uiop:getenv "POIU_TEST_SYSTEM")
      (first (uiop:command-line-arguments))
      (error "Provide a system name via POIU_TEST_SYSTEM or argv")))

(defun canonical-system-name (designator)
  (string-downcase (string designator)))

(defun ensure-system-available (name)
  (or (asdf:find-system name nil)
      (error "System ~A is not available in this environment" name)))

(defun ensure-system-output-directory (system)
  (let ((source-file (asdf:system-source-file system)))
    (when source-file
      (ensure-output-directory-for-source source-file))))

(defun main ()
  (ensure-local-cache)
  (pushnew *repo-root* asdf:*central-registry* :test #'equal)
  (maybe-load-quicklisp)
  (maybe-load-ocicl)
  (asdf:load-system :poiu)
  (maybe-set-max-forks)
  (let* ((name (canonical-system-name (requested-system-name)))
         (system (ensure-system-available name)))
    (ensure-system-output-directory system)
    (format t "loading-system=~A~%" name)
    (asdf:load-system name :force t :verbose nil)
    (format t "PASS ~A~%" name)))

(main)
