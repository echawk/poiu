(uiop:define-package :poiu/background-process
  (:use :uiop/common-lisp
        :uiop/utility :uiop/stream :uiop/pathname :uiop/filesystem
        :uiop/lisp-build :uiop/image
        :poiu/queue :poiu/fork)
  (:export #:worker #:worker-pid #:worker-job #:worker-generation #:worker-idle-p
           #:start-worker #:assign-job #:finished-workers #:retire-worker #:retire-workers
           #:job #:job-item #:job-result #:job-condition
           #:replay-job-output #:discard-job-output
           #:process-failed
           #:call-with-result-directory #:*time-spent-waiting*))
(in-package :poiu/background-process)

;;; Background workers
;;
;; A worker is a forked copy of the main process that performs jobs on its behalf.
;; Forking is expensive for a large image (on macOS, it costs time proportional to the
;; *reserved* heap size), so a worker performs one job after another, as long as the
;; main process deems its copy of the image to be recent enough for the next job.
;;
;; The main process sends a worker a job as a line "<job-number> <item-index>" on a pipe,
;; where the item index refers to a vector of items that the worker inherited when forked.
;; The worker performs the job, writes its result and output in files named after the job,
;; then reports completion by writing "<job-number>" on another pipe.

(defvar *time-spent-waiting* 0
  "Internal real time units the parent spent idle, waiting for background workers.")

(define-condition process-failed (error)
  ((exit-status :initform nil :initarg :exit-status :reader process-failed-exit-status)
   (condition-type :initform nil :initarg :condition-type :reader process-failed-condition-type)
   (condition :initform nil :initarg :condition :reader process-failed-condition-message))
  (:report
   (lambda (condition stream)
     (cond
       ((process-failed-condition-message condition)
        (princ (process-failed-condition-message condition) stream))
       ((process-failed-exit-status condition)
        (format stream "Background process exited with status ~A"
                (process-failed-exit-status condition)))
       (t
        (princ "Background process failed" stream))))))

(defvar *result-directory* nil
  "Private directory where background workers write their results.")
(defvar *job-counter* 0)

(defun call-with-result-directory (thunk)
  (let ((directory
          (ensure-directory-pathname
           (subpathname (temporary-directory)
                        (format nil "poiu-~36R~36R/" (get-universal-time) (random (expt 36 8)))))))
    (ensure-directories-exist directory)
    (unwind-protect
         (let ((*result-directory* directory)
               (*job-counter* 0))
           (funcall thunk))
      (ignore-errors (delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defun job-file (number type)
  (subpathname *result-directory* (format nil "~D.~A" number type)))

(defclass job ()
  ((number :initarg :number :reader job-number)
   (item :initarg :item :reader job-item)
   (result :initform nil :accessor job-result)
   (condition :initform nil :accessor job-condition)))

(defclass worker ()
  ((pid :initarg :pid :reader worker-pid)
   (generation :initarg :generation :reader worker-generation
               :documentation "an opaque value describing the state of the main process
when the worker was forked")
   (commands :initarg :commands :reader worker-commands
             :documentation "output stream on which to send jobs to the worker")
   (acks :initarg :acks :reader worker-acks
         :documentation "input stream on which the worker reports completed jobs")
   (job :initform nil :accessor worker-job
        :documentation "the job the worker is performing, if any")))

(defun worker-idle-p (worker)
  (null (worker-job worker)))

;;; In the worker

(defun child-exit (code)
  ;; Never unwind or run exit hooks in a forked child: the parent owns all the state we
  ;; inherited (buffered streams, temporary files, finalizers), so exit immediately.
  (ignore-errors (finish-outputs))
  (quit code nil))

(defun write-job-result (number result condition)
  (with-open-file (s (job-file number "sexp")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-safe-io-syntax ()
      (write (reify-simple-sexp
              `(:process-done
                ,@(when result `(:result ,result))
                ,@(when condition
                    `(:condition-type ,(princ-to-string (class-name (class-of condition)))
                      :condition ,(or (ignore-errors (princ-to-string condition))
                                      "Unprintable condition")))))
             :stream s))))

(defun perform-job (number item function)
  "Call FUNCTION on ITEM, recording its result and output in the files of job NUMBER.
Return true if it succeeded."
  (reset-deferred-warnings)
  (with-open-file (out (job-file number "out") :direction :output :if-exists :supersede)
    (with-open-file (err (job-file number "err") :direction :output :if-exists :supersede)
      (let ((*standard-output* out)
            (*error-output* err)
            (*trace-output* out))
        (multiple-value-bind (result condition)
            (handler-case (values (funcall function item) nil)
              (serious-condition (c) (values nil c)))
          (write-job-result number result condition)
          (null condition))))))

(defun worker-loop (items function commands acks)
  (loop
    (let ((line (read-line commands nil nil)))
      (unless line (return)) ; the main process is done with us
      (with-input-from-string (s line)
        (let* ((number (read s))
               (index (read s))
               (successp (perform-job number (aref items index) function)))
          (write-line (princ-to-string number) acks)
          (finish-output acks)
          ;; Don't reuse an image in which something went wrong.
          (unless successp (return)))))))

;;; In the main process

(defun start-worker (items function generation other-workers)
  "Fork a worker that will call FUNCTION on elements of the vector ITEMS, as requested by
ASSIGN-JOB. GENERATION is recorded as the WORKER-GENERATION. OTHER-WORKERS are the existing
workers, whose pipes the new worker must not keep open."
  (disable-other-waiters)
  (finish-outputs)
  (multiple-value-bind (command-in command-out) (posix-pipe)
    (multiple-value-bind (ack-in ack-out) (posix-pipe)
      (let ((pid (posix-fork)))
        (cond
          ((zerop pid) ; in the child
           (unwind-protect
                (progn
                  ;; don't receive the parent's SIGINTs
                  (posix-setpgrp)
                  #+sbcl
                  (progn
                    (sb-ext:disable-debugger)
                    (when (find-package :sb-sprof)
                      (funcall (intern "STOP-PROFILING" :sb-sprof))))
                  #+clozure (setf ccl::*batch-flag* t)
                  ;; Close the main process's ends of all pipes, so that each worker
                  ;; sees end of file when the main process closes its end, and vice versa.
                  (dolist (stream (list* command-out ack-in
                                         (loop :for w :in other-workers
                                               :append (list (worker-commands w)
                                                             (worker-acks w)))))
                    (ignore-errors (close stream :abort t)))
                  (worker-loop items function command-in ack-out)
                  (child-exit 0))
             (child-exit 1)))
          (t ; in the parent
           (close command-in :abort t)
           (close ack-out :abort t)
           (make-instance 'worker :pid pid :generation generation
                                  :commands command-out :acks ack-in)))))))

(defun assign-job (worker item index)
  "Have the idle WORKER perform its function on ITEM, which is at INDEX in its items vector."
  (assert (worker-idle-p worker))
  (let ((job (make-instance 'job :number (incf *job-counter*) :item item)))
    (setf (worker-job worker) job)
    (handler-case
        (progn
          (format (worker-commands worker) "~D ~D~%" (job-number job) index)
          (finish-output (worker-commands worker)))
      ;; If the worker died, we'll find out when we look for finished workers.
      (error () nil))
    job))

(defun read-job-result (job)
  (multiple-value-bind (form condition)
      (ignore-errors
       (with-open-file (s (job-file (job-number job) "sexp")
                          :direction :input :if-does-not-exist :error)
         ;; Keep the result reified: unreifying would intern its symbols in the main process,
         ;; possibly before their package is fully set up. See UNREIFY-SIMPLE-SEXP.
         (with-safe-io-syntax ()
           (read s))))
    (ignore-errors (delete-file (job-file (job-number job) "sexp")))
    (cond
      (condition
       (setf (job-condition job)
             (make-condition 'process-failed :condition "Could not read result file")))
      ((not (and (consp form) (eq (car form) :process-done)))
       (setf (job-condition job)
             (make-condition 'process-failed :condition "Invalid result file")))
      (t
       (destructuring-bind (&key result condition condition-type) (cdr form)
         (setf (job-result job) result)
         (when condition
           (setf (job-condition job)
                 (make-condition 'process-failed
                                 :condition condition
                                 :condition-type condition-type))))))))

(defun worker-exited-p (worker)
  "Has the WORKER exited? If so, reap it."
  (let ((pid (posix-waitpid (worker-pid worker) :nohang t)))
    (or (eql pid (worker-pid worker)) (eql pid -1))))

(defun finished-workers (workers &key wait)
  "Return two values: the busy WORKERS that completed their job, which is available
as their WORKER-JOB, and the WORKERS that died, after marking their job, if any, as failed.
If WAIT is true, block until at least one busy worker completes or dies."
  (let ((busy (remove-if #'worker-idle-p workers))
        (start (get-internal-real-time)))
    (unwind-protect
         (loop
           (let ((readable (wait-for-input (mapcar #'worker-acks busy) (if wait 0.1 0)))
                 (completed '())
                 (dead '()))
             (dolist (worker busy)
               (when (member (worker-acks worker) readable)
                 (let ((line (ignore-errors (read-line (worker-acks worker) nil nil))))
                   (if line
                       (let ((job (worker-job worker)))
                         (assert (eql (parse-integer line :junk-allowed t) (job-number job)))
                         (read-job-result job)
                         (push worker completed))
                       ;; End of file: the worker died before completing its job.
                       (push worker dead)))))
             (when (null readable)
               ;; Safety net, in case a dead worker's pipe isn't seen at end of file.
               (dolist (worker busy)
                 (when (worker-exited-p worker)
                   (push worker dead))))
             (dolist (worker dead)
               (when (worker-job worker)
                 (setf (job-condition (worker-job worker))
                       (make-condition 'process-failed :condition "Background worker died")))
               (ignore-errors (close (worker-commands worker) :abort t))
               (ignore-errors (close (worker-acks worker) :abort t)))
             (when (or completed dead (not wait) (null busy))
               (return (values (nreverse completed) (nreverse dead))))))
      (incf *time-spent-waiting* (- (get-internal-real-time) start)))))

(defun replay-job-output (job)
  "Copy what the JOB printed to the current output streams, then delete it."
  (loop :for (type stream) :in `(("out" ,*standard-output*) ("err" ,*error-output*))
        :for file = (job-file (job-number job) type)
        :do (ignore-errors
             (with-open-file (in file :direction :input :if-does-not-exist nil)
               (when in (copy-stream-to-stream in stream))))
            (ignore-errors (delete-file-if-exists file)))
  (finish-outputs))

(defun discard-job-output (job)
  "Forget what the JOB printed."
  (dolist (type '("out" "err"))
    (ignore-errors (delete-file-if-exists (job-file (job-number job) type)))))

(defun retire-worker (worker)
  "Terminate the WORKER and reap it. Any job it was performing is abandoned."
  (ignore-errors (close (worker-commands worker) :abort t))
  (ignore-errors (close (worker-acks worker) :abort t))
  ;; SIGKILL: the worker holds nothing worth saving.
  (posix-kill (worker-pid worker) 9)
  (ignore-errors (posix-waitpid (worker-pid worker))))

(defun retire-workers (workers)
  (map () 'retire-worker workers))
