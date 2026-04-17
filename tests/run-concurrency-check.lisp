(in-package #:cl-user)

(defparameter *tests-root*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename* *default-pathname-defaults*)))

(defparameter *repo-root*
  (uiop:merge-pathnames* #p"../" *tests-root*))

(require :asdf)
(pushnew *repo-root* asdf:*central-registry* :test #'equal)
(asdf:load-system :poiu)

(defparameter *log-file*
  (uiop:merge-pathnames* #p"concurrency-events.log" *tests-root*))

(defparameter *cache-root*
  (uiop:merge-pathnames* #p".cache/" *repo-root*))

(defparameter *target-root*
  (uiop:merge-pathnames* #p"concurrency-target/" *tests-root*))

(defun parse-event-line (line)
  (with-input-from-string (stream line)
    (list (read stream nil nil)
          (read stream nil nil)
          (read stream nil nil))))

(defun load-events (pathname)
  (with-open-file (stream pathname)
    (loop :for line = (read-line stream nil nil)
          :while line
          :collect (parse-event-line line))))

(defun compute-peak-concurrency (events)
  (let ((boundaries
          (sort
           (loop :for (kind file timestamp) :in events
                 :collect (list timestamp
                                (ecase kind
                                  (compile-start 1)
                                  (compile-end -1))
                                file))
           #'< :key #'first))
        (current 0)
        (peak 0))
    (dolist (boundary boundaries peak)
      (destructuring-bind (_time delta _file) boundary
        (declare (ignore _time _file))
        (incf current delta)
        (setf peak (max peak current))))))

(defun ensure-local-cache ()
  (ensure-directories-exist *cache-root*)
  (unless (uiop:getenv "XDG_CACHE_HOME")
    (error "XDG_CACHE_HOME must be set by the shell wrapper")))

(defun verify-result ()
  (let ((result (funcall (uiop:find-symbol* :run :poiu-test-target))))
    (unless (equal result '(:loaded :loaded :loaded :loaded))
      (error "Unexpected target result: ~S" result))))

(defun main ()
  (ensure-local-cache)
  (uiop:delete-file-if-exists *log-file*)
  (unless (equal (uiop:getenv "POIU_TEST_LOG") (namestring *log-file*))
    (error "POIU_TEST_LOG must be set to ~A" (namestring *log-file*)))
  (pushnew *repo-root* asdf:*central-registry* :test #'equal)
  (pushnew *target-root* asdf:*central-registry* :test #'equal)
  (let* ((poiu/fork:*max-forks* 4)
         (poiu/fork:*max-actual-forks* 0)
         (start (get-internal-real-time)))
    (asdf:load-system :poiu-test-target :force t)
    (verify-result)
    (let* ((elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second))
           (events (load-events *log-file*))
           (peak (compute-peak-concurrency events)))
      (format t "XDG_CACHE_HOME=~A~%" (uiop:getenv "XDG_CACHE_HOME"))
      (format t "elapsed=~,2F seconds~%" elapsed)
      (format t "peak_compile_concurrency=~D~%" peak)
      (format t "max_actual_forks=~D~%" poiu/fork:*max-actual-forks*)
      (format t "event_count=~D~%" (length events))
      (unless (= (length events) 8)
        (error "Expected 8 compile events, got ~D" (length events)))
      (unless (> peak 1)
        (error "Expected compile overlap, got peak concurrency ~D" peak))
      (unless (> poiu/fork:*max-actual-forks* 1)
        (error "Expected multiple worker processes, got ~D" poiu/fork:*max-actual-forks*))
      (unless (< elapsed 6.5)
        (error "Expected a parallel build to finish in under 6.5s, got ~,2Fs" elapsed))
      (format t "PASS~%"))))

(main)
