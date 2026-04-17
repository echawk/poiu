(in-package #:poiu-test-target)

(eval-when (:compile-toplevel)
  (let ((log (uiop:getenv "POIU_TEST_LOG")))
    (when log
      (with-open-file (stream log :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream "~A ~A ~A~%"
                "compile-start"
                "file-c"
                (/ (get-internal-real-time) internal-time-units-per-second))
        (force-output stream))))
  (sleep 2)
  (let ((log (uiop:getenv "POIU_TEST_LOG")))
    (when log
      (with-open-file (stream log :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream "~A ~A ~A~%"
                "compile-end"
                "file-c"
                (/ (get-internal-real-time) internal-time-units-per-second))
        (force-output stream)))))

(defparameter *file-c* :loaded)
