(uiop:define-package :poiu/background-process
  (:use :uiop/common-lisp
        :uiop/utility :uiop/stream
        :uiop/lisp-build :uiop/image
        :poiu/queue :poiu/fork)
  (:export #:doqueue/forking))
(in-package :poiu/background-process)

;;; Timing the build process

(defvar *time-spent-waiting* 0)

(defmacro timed-do ((time-accumulator) &body body)
  (let ((time-before-thing (gensym)))
    `(let ((,time-before-thing (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
              (incf ,time-accumulator (- (get-internal-real-time)
                                         ,time-before-thing))))))

;;; Handling multiple processes: high-level API

(defclass background-process ()
  ((pid :initarg :pid :accessor process-pid)
   (data :initarg :data :accessor process-data)
   (cleanup :initarg :cleanup :accessor process-cleanup)
   ;; We pass results through a file: pipes may cause deadlocks due to full buffers and naive event loop.
   (result-file :initarg :result-file :accessor process-result-file)))

(define-condition process-failed (error)
  ((exit-status :initarg :exit-status :reader process-failed-exit-status)
   (condition-type :initform nil :initarg :condition-type :reader process-failed-condition-type)
   (condition :initform nil :initarg :condition :reader process-failed-condition-message))
  (:report
   (lambda (condition stream)
     (cond
       ((process-failed-exit-status condition)
        (format stream "Background process exited with status ~A"
                (process-failed-exit-status condition)))
       ((process-failed-condition-message condition)
        (princ (process-failed-condition-message condition) stream))
       (t
        (princ "Background process failed" stream))))))

(defun process-return (result-file result condition)
  (with-open-file (s result-file
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-safe-io-syntax ()
      (write (reify-simple-sexp
              `(:process-done
                ,@(when result `(:result ,result))
                ,@(when condition
                    `(:condition-type ,(princ-to-string (class-name (class-of condition)))
                      :condition ,(princ-to-string condition)))))
             :stream s))))

(defun process-result (process status)
  (block nil
    (when status
      (let ((exit-status (posix-wexitstatus status)))
        (unless (zerop exit-status)
          (return (values nil (make-condition 'process-failed :exit-status exit-status))))))
    (multiple-value-bind (form condition)
        (ignore-errors
         (with-open-file (s (process-result-file process)
                            :direction :input :if-does-not-exist :error)
           (with-safe-io-syntax ()
             (unreify-simple-sexp (read s)))))
      (when condition
        (return (values nil (make-condition 'process-failed :condition "Could not read result file"))))
      (unless (and (consp form) (eq (car form) :process-done))
        (return (values nil (make-condition 'process-failed :condition "Invalid result file"))))
      (destructuring-bind (&key result condition condition-type) (cdr form)
        (return
          (values result
                  (when condition
                    (make-condition 'process-failed
                                    :condition condition
                                    :condition-type condition-type))))))))

(defun make-background-process (data function cleanup result-file)
  (disable-other-waiters)
  (finish-outputs)
  (let ((pid (posix-fork)))
    (cond
      ((zerop pid) ; in the child
       ;; don't receive the parent's SIGINTs
       (posix-setpgrp)
       #+sbcl
       (progn
         (sb-ext:disable-debugger)
         (when (find-package :sb-sprof)
           (funcall (intern "STOP-PROFILING" :sb-sprof))))
       #+clozure (setf ccl::*batch-flag* t)
       (reset-deferred-warnings)
       (unwind-protect
            (multiple-value-bind (result condition)
                (ignore-errors
                  (with-compilation-unit ()
                    (values (funcall function data t))))
              (process-return result-file result condition))
         (finish-outputs)
         (quit 0 t)))
      (t ; in the parent
       (make-instance 'background-process
                      :pid pid
                      :result-file result-file
                      :cleanup cleanup
                      :data data)))))

(defun call-queue/forking (fun fg-queue bg-queue
                           &key announce cleanup result-file deterministic-order)
  ;; assumes a single-threaded parent process
  (declare (optimize debug))
  (let ((processes (make-hash-table :test 'equal)))
    (labels ((fg-perform (action)
               (funcall announce action nil)
               (multiple-value-bind (result condition)
                   (ignore-errors (values (funcall fun action nil)))
                 (funcall cleanup action result condition nil)))
             (cleanup-one (process status)
               (multiple-value-bind (result condition)
                   (process-result process status)
                 (funcall (process-cleanup process)
                          (process-data process) result condition t)))
             (reap (&key wait)
               (disable-other-waiters)
               (multiple-value-bind (pid status)
                   (timed-do (*time-spent-waiting*)
                     (posix-waitpid -1 :nohang (not wait)))
                 (etypecase pid
                   ((eql 0)
                    nil)
                   ((integer 1 *)
                    (let ((process (gethash pid processes)))
                      (assert process ()
                              "couln't find the pid ~A in processes ~S"
                              pid (table-values processes))
                      (remhash pid processes)
                      (cleanup-one process status))
                    t)
                   ((eql -1)
                    (assert (eql status +echild+) (status))
                    ;; Our implementation or some library may have disabled SIGCHLD
                    ;; or preempted our wait. Mark all children as completed.
                    (let ((missed (table-values processes)))
                      (warn "No child left: we must have dropped a signal!")
                      (clrhash processes)
                      (dolist (process missed)
                        (cleanup-one process nil)))
                    t))))
             (run-foreground-action ()
               (if deterministic-order
                   ;; Run a single action at a time so newly-enabled in-image work can
                   ;; update the phase boundary before we commit to later compiles.
                   (let* ((ordered-actions
                            (sort (dequeue-all fg-queue) #'< :key deterministic-order))
                          (next-action (first ordered-actions)))
                     (dolist (deferred-action (rest ordered-actions))
                       (enqueue fg-queue deferred-action))
                     (fg-perform next-action))
                   (fg-perform (dequeue fg-queue)))))
      (loop
        (let* ((no-fg-item? (empty-p fg-queue))
               (fg-item? (not no-fg-item?))
               (no-bg-item? (empty-p bg-queue))
               (bg-item? (not no-bg-item?))
               (no-processes? (empty-p processes))
               (processes? (not no-processes?))
               (worker-slot-open-p (< (size processes) *max-forks*))
               (can-fork-background-action-p (and bg-item? worker-slot-open-p))
               (can-run-foreground-action-p
                 (and fg-item?
                      (or (not deterministic-order)
                          no-bg-item?)))
               (must-wait-for-process-p
                 (and processes?
                      (not can-fork-background-action-p)
                      (not can-run-foreground-action-p))))
          (cond
            ((and processes?
                  (reap :wait must-wait-for-process-p)))
            (can-fork-background-action-p
             (let ((item (dequeue bg-queue)))
               (funcall announce item t)
               (let ((process
                       (make-background-process item fun cleanup (funcall result-file item))))
                 (setf (gethash (process-pid process) processes) process)
                 (when (< *max-actual-forks* (size processes))
                   (setf *max-actual-forks* (size processes))))))
            (can-run-foreground-action-p
             (run-foreground-action))
            ((and no-fg-item? no-bg-item? no-processes?)
             (return))
            (t
             (assert nil
                     (fg-item? bg-item? processes? worker-slot-open-p
                      can-fork-background-action-p can-run-foreground-action-p
                      must-wait-for-process-p)
                     "Unreachable scheduler state"))))))))

(defmacro doqueue/forking ((fg-queue bg-queue
                            &key variables deterministic-order
                              (announce nil) (cleanup nil) result-file)
                           &body body)
  (destructuring-bind (&key item backgroundp result condition) variables
    `(call-queue/forking
      #'(lambda (,item ,backgroundp) (declare (ignorable ,item ,backgroundp)) ,@body)
      ,fg-queue ,bg-queue
      :deterministic-order ,deterministic-order
      :result-file #'(lambda (,item) (declare (ignorable ,item)) ,result-file)
      :announce #'(lambda (,item ,backgroundp) (declare (ignorable ,item ,backgroundp)) ,announce)
      :cleanup #'(lambda (,item ,result ,condition ,backgroundp)
                   (declare (ignorable ,item ,result ,condition ,backgroundp)) ,cleanup))))
