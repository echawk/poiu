(in-package #:cl-user)

(require :asdf)
#+sbcl
(require :sb-posix)

(defparameter *tests-root*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename* *default-pathname-defaults*)))

(defparameter *repo-root*
  (uiop:merge-pathnames* #p"../" *tests-root*))

(defun requested-plan-mode ()
  (let ((value (or (uiop:getenv "POIU_PLAN_MODE") "parallel")))
    (cond
      ((string-equal value "parallel") :parallel)
      ((string-equal value "sequential") :sequential)
      (t
       (error "POIU_PLAN_MODE must be one of PARALLEL or SEQUENTIAL, got ~S" value)))))

(defun plan-mode-name (mode)
  (ecase mode
    (:parallel "parallel")
    (:sequential "sequential")))

(defun parallel-mode-p (mode)
  (eq mode :parallel))

(defun current-process-id ()
  #+sbcl
  (sb-posix:getpid)
  #-sbcl
  nil)

(defun ensure-local-cache ()
  (let ((cache-root (uiop:getenv "XDG_CACHE_HOME")))
    (unless cache-root
      (error "XDG_CACHE_HOME must be set by the shell wrapper"))
    (ensure-directories-exist (uiop:ensure-directory-pathname cache-root))))

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

(defun maybe-load-poiu (mode)
  (when (parallel-mode-p mode)
    (asdf:load-system :poiu)
    (maybe-set-max-forks)))

(defun report-elapsed-time (start root-process-id)
  (when (or (null root-process-id)
            (= root-process-id (current-process-id)))
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)))
      (format t "elapsed=~,2F seconds~%" elapsed)
      (finish-output))))

(defun load-system-with-timing (name mode)
  (let* ((system (ensure-system-available name))
         (start (get-internal-real-time))
         (root-process-id (current-process-id)))
    (format t "mode=~A~%" (plan-mode-name mode))
    (format t "loading-system=~A~%" name)
    (finish-output)
    (ensure-system-output-directory system)
    (unwind-protect
         (handler-case
             (progn
               (asdf:load-system name :force t :verbose nil)
               (format t "PASS ~A~%" name)
               (finish-output))
           (error (condition)
             (format t "FAIL ~A~%" name)
             (finish-output)
             (error condition)))
      (report-elapsed-time start root-process-id))))

(defun main ()
  (ensure-local-cache)
  (pushnew *repo-root* asdf:*central-registry* :test #'equal)
  (maybe-load-quicklisp)
  (maybe-load-ocicl)
  (let* ((mode (requested-plan-mode))
         (name (canonical-system-name (requested-system-name))))
    (maybe-load-poiu mode)
    (load-system-with-timing name mode)))

(main)
