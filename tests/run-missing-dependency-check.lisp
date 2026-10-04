;;; Check that POIU correctly builds a system whose files lack some dependencies,
;;; but that builds sequentially thanks to the order of its components.
(in-package #:cl-user)
(require :asdf)
(defparameter *tests-root*
  (make-pathname :name nil :type nil :defaults (or *load-truename* *compile-file-truename*)))
(push (uiop:merge-pathnames* #p"../" *tests-root*) asdf:*central-registry*)
(push (uiop:merge-pathnames* #p"missing-dependency-target/" *tests-root*) asdf:*central-registry*)
(asdf:load-system :poiu)
(setf poiu:*parallel-plan-deterministic-p*
      (not (equal (uiop:getenv "POIU_DETERMINISTIC") "0")))
(setf poiu/fork:*max-forks* 4)
(asdf:load-system :poiu-missing-dependency-target :force t :verbose t)
(let ((result (uiop:symbol-call :poiu-missing-dependency-target :run))
      (stats poiu:*last-build-statistics*))
  (format t "~&deterministic=~A result=~S stats=~S~%"
          poiu:*parallel-plan-deterministic-p* result stats)
  (unless (equal result '(42 42 42))
    (error "Expected (42 42 42), got ~S" result))
  (unless (plusp (getf stats :retried))
    (error "Expected some actions to be retried in the foreground"))
  (format t "PASS~%"))
