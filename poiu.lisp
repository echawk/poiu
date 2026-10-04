(uiop:define-package :poiu
  (:use :uiop/common-lisp :uiop
        :poiu/queue :poiu/fork :poiu/background-process
        :asdf/session :asdf/upgrade
        :asdf/component :asdf/system :asdf/find-component :asdf/operation
        :asdf/action :asdf/plan :asdf/operate
        :asdf/output-translations)
  (:use-reexport :poiu/action-graph)
  (:export #:*last-build-statistics* #:*last-build-timeline*))

(in-package :poiu)

;;; Choosing when to use a parallel plan

(defun parallel-plan-class-p (plan-class)
  (let ((class (if (typep plan-class 'class) plan-class (find-class plan-class nil))))
    (and class (subtypep class (find-class 'parallel-plan)))))

(defun nested-plan-p ()
  "Is a plan being made from within the traversal or PERFORM of another action?
Such nested plans (e.g. from OPERATE in a .asd file or a PERFORM method) are
performed sequentially: their context belongs to another plan, and they may
run inside a background process."
  (and *asdf-session* (visiting-action-list *asdf-session*) t))

(defmethod make-plan :around (plan-class (o operation) (c component) &rest keys &key &allow-other-keys)
  (if (and (parallel-plan-class-p (or plan-class *plan-class*))
           (nested-plan-p))
      (apply 'make-plan 'sequential-plan o c keys)
      (call-next-method)))

;;; Performing a parallel plan

(defvar *last-build-statistics* nil
  "A plist describing the last parallel build: actions performed in the foreground and
the background, the number of workers forked and the peak number alive at once, and timings.")

(defvar *last-build-timeline* nil
  "For the last parallel build, a list of (action-description where start end) for each
action performed, where WHERE is :FOREGROUND, :BACKGROUND or :RETRY, and START and END are
in seconds since the build started.")

(defun background-action-p (plan action)
  "Can ACTION be performed in a forked process? Only actions that produce files can:
actions that are needed in the image (e.g. loading) must happen in the main process."
  (let ((o (action-operation action))
        (c (action-component action)))
    (and (plusp *max-forks*)
         (not (needed-in-image-p o c))
         (not (action-already-done-p plan o c)))))

(defgeneric ordered-action-p (operation component)
  (:documentation "Must this in-image action be performed in plan order in a deterministic build?
Actions that merely gate other actions, such as PREPARE-OP, can be performed as soon as their
dependencies are, which lets compilation of independent files start early.")
  (:method ((o operation) (c component))
    (needed-in-image-p o c))
  (:method ((o asdf:prepare-op) (c component))
    nil)
  (:method ((o asdf:prepare-source-op) (c component))
    nil))

;;; Deferred warnings: warnings that the compiler defers until the end of the compilation unit,
;;; such as undefined functions, must be passed from the workers to the main process.
;;; UIOP's REIFY-DEFERRED-WARNINGS doesn't support recent SBCLs, which keep the counts of
;;; warnings in a COMPILATION-UNIT structure; we support them here.

(defun sbcl-compilation-unit-struct-p ()
  #+sbcl (not (boundp (find-symbol* '#:*in-compilation-unit* :sb-c)))
  #-sbcl nil)

(defparameter +compilation-unit-counts+
  '(#:aborted-count #:error-count #:warning-count #:style-warning-count #:note-count))

(defun compilation-unit-count-slot (name)
  (find-symbol* name :sb-c))

(defun reset-worker-deferred-warnings ()
  (if (sbcl-compilation-unit-struct-p)
      #+sbcl
      (let ((unit (symbol-value (find-symbol* '#:*compilation-unit* :sb-c))))
        (setf sb-c::*undefined-warnings* nil)
        (when unit
          (dolist (name +compilation-unit-counts+)
            (setf (slot-value unit (compilation-unit-count-slot name)) 0))))
      #-sbcl nil
      (reset-deferred-warnings)))

(defun reify-worker-deferred-warnings ()
  "Return a simple S-expression representing the warnings deferred in the current compilation
unit, in the format of REIFY-DEFERRED-WARNINGS if possible."
  (if (sbcl-compilation-unit-struct-p)
      #+sbcl
      (let ((unit (symbol-value (find-symbol* '#:*compilation-unit* :sb-c))))
        (when unit
          `(,@(when sb-c::*undefined-warnings*
                ;; The contexts of each warning were already printed when it was signaled.
                `((sb-c::*undefined-warnings*
                   ,@(loop :for warning :in sb-c::*undefined-warnings*
                           :collect (list (sb-c::undefined-warning-kind warning)
                                          (sb-c::undefined-warning-name warning)
                                          (sb-c::undefined-warning-count warning))))))
            ,@(loop :for name :in +compilation-unit-counts+
                    :for value = (slot-value unit (compilation-unit-count-slot name))
                    :when (plusp value)
                      :collect `(,(intern (symbol-name name) :keyword) . ,value)))))
      #-sbcl nil
      (ignore-errors (reify-deferred-warnings))))

(defun defined-now-p (kind name)
  "Is the thing of KIND named NAME, reported as undefined while compiling some file, defined now?"
  (ignore-errors
   (case kind
     (:function (fboundp name))
     (:variable (or (boundp name)
                    #+sbcl (member (sb-int:info :variable :kind name) '(:special :global :constant))))
     (:type (or (find-class name nil)
                #+sbcl (sb-int:info :type :kind name)))
     (t nil))))

(defun unreify-worker-deferred-warnings (reified)
  "Add the deferred warnings REIFIED by REIFY-WORKER-DEFERRED-WARNINGS (and still reified as per
REIFY-SIMPLE-SEXP) to the current compilation unit, except for things defined since."
  (let ((warnings (unreify-simple-sexp reified)))
    (if (sbcl-compilation-unit-struct-p)
        #+sbcl
        (let ((unit (symbol-value (find-symbol* '#:*compilation-unit* :sb-c))))
          (dolist (item warnings)
            (destructuring-bind (key . value) item
              (cond
                ((eq key 'sb-c::*undefined-warnings*)
                 (loop :for (kind name count) :in value
                       :for existing = (find-if #'(lambda (w)
                                                    (and (eq (sb-c::undefined-warning-kind w) kind)
                                                         (equal (sb-c::undefined-warning-name w)
                                                                name)))
                                                sb-c::*undefined-warnings*)
                       :unless (defined-now-p kind name)
                         :do (if existing
                                 (incf (sb-c::undefined-warning-count existing) count)
                                 (push (sb-c::make-undefined-warning
                                        :kind kind :name name :count count)
                                       sb-c::*undefined-warnings*))))
                ((and unit (keywordp key))
                 (incf (slot-value unit (compilation-unit-count-slot key)) value))))))
        #-sbcl nil
        (unreify-deferred-warnings warnings))))

;;; Missing dependencies: a file that uses a definition from a file it doesn't depend on may be
;;; compiled by a worker in which that definition doesn't exist yet. Usually this causes an error,
;;; and the file is compiled again in the main process; but in some cases, the compiler silently
;;; generates different code: a macro call becomes a call to an undefined function, a binding of
;;; a special variable becomes a lexical binding, etc. To catch these cases, the worker notes the
;;; relevant warnings when compiling each file, and before loading the file, the main process
;;; checks whether the missing definitions now exist; if any does, it compiles the file again.
;;; Results from workers are kept reified (see REIFY-SIMPLE-SEXP) until the end of the build,
;;; so as not to intern symbols in the main process, and names are only looked up.
(defun reified-symbol-lookup (reified)
  "The symbol in the current image for a REIFIED symbol, if it exists, without interning it."
  (typecase reified
    (symbol reified)
    ((simple-vector 2)
     (let ((name (svref reified 0))
           (package (svref reified 1)))
       (and (stringp name)
            (stringp package)
            (find-package package)
            (values (find-symbol name package)))))))

(defun reified-undefined (kind reified-deferred-warnings)
  "The names of the undefined things of KIND (e.g. :FUNCTION or :VARIABLE) in
REIFIED-DEFERRED-WARNINGS, still reified."
  #+(or sbcl cmucl scl)
  (loop :for item :in reified-deferred-warnings
        :for key = (and (consp item) (reified-symbol-lookup (car item)))
        :when (and key (symbolp key) (string= (symbol-name key) "*UNDEFINED-WARNINGS*"))
          :append (loop :for warning :in (cdr item)
                        :when (and (consp warning) (eq (first warning) kind))
                          :collect (second warning)))
  #-(or sbcl cmucl scl)
  (progn kind reified-deferred-warnings nil))

(defun setf-expander-p (symbol)
  (and (symbolp symbol)
       (not (fboundp `(setf ,symbol)))
       #+sbcl (sb-int:info :setf :expander symbol)
       #-sbcl nil))

(defun globally-special-p (symbol)
  (and (symbolp symbol)
       #+sbcl (eq (sb-int:info :variable :kind symbol) :special)
       #-sbcl (boundp symbol)))

(defun symbol-macro-p (symbol)
  (and (symbolp symbol)
       (nth-value 1 (macroexpand-1 symbol))))

(defun now-defined-missing-definitions (missing)
  "MISSING is a plist of reified names that were undefined or treated as lexical when a worker
compiled some file. Return a list of descriptions of those that would have made a difference."
  (destructuring-bind (&key functions variables lexicals) missing
    (append
     (loop :for reified :in functions
           :for name = (if (consp reified)
                           (list (first reified) (reified-symbol-lookup (second reified)))
                           (reified-symbol-lookup reified))
           :when (and (symbolp name) name (macro-function name))
             :collect (format nil "the macro ~S" name)
           :when (and (consp name) (eq (first name) 'setf) (setf-expander-p (second name)))
             :collect (format nil "the setf expander for ~S" (second name)))
     (loop :for reified :in variables
           :for name = (reified-symbol-lookup reified)
           :when (and name (symbol-macro-p name))
             :collect (format nil "the symbol macro ~S" name))
     (loop :for reified :in lexicals
           :for name = (reified-symbol-lookup reified)
           :when (and name (globally-special-p name))
             :collect (format nil "the special declaration of ~S" name)))))

(defun lexical-binding-of-special-name-p (condition)
  "Is CONDITION SBCL's style-warning about the lexical binding of a variable named *LIKE-THIS*?
If so, return the variable name."
  (when (and (typep condition 'simple-condition)
             (let ((class (find-symbol* '#:asterisks-around-lexical-variable-name
                                        :sb-kernel nil))
                   (control (simple-condition-format-control condition)))
               (or (and class (typep condition class))
                   (and (stringp control)
                        (search "using the lexical binding of the symbol" control)))))
    (let ((name (first (simple-condition-format-arguments condition))))
      (and (symbolp name) name))))

(defun mark-no-op-dependencies-done (operation component)
  "In a worker, mark as done the dependencies of the action that the main process performed
after forking the worker, which can only be actions that don't modify the image, such as
PREPARE-OP, so that ASDF knows they are done when computing the action's timestamp."
  (map-direct-dependencies
   operation component
   #'(lambda (o c)
       (unless (or (nth-value 1 (component-operation-time o c))
                   (ordered-action-p o c))
         (mark-no-op-dependencies-done o c)
         (mark-operation-done o c)))))

(defun perform-in-background (action)
  "What a worker does: perform the ACTION and report deferred warnings,
as well as the names of variables bound lexically while named like special variables."
  (let ((o (action-operation action))
        (c (action-component action))
        (lexicals '()))
    (mark-no-op-dependencies-done o c)
    (reset-worker-deferred-warnings)
    (handler-bind ((style-warning
                     #'(lambda (condition)
                         (if-let (name (lexical-binding-of-special-name-p condition))
                           (pushnew name lexicals)))))
      (perform-with-restarts o c))
    (let ((deferred-warnings (reify-worker-deferred-warnings)))
      `(,@(when deferred-warnings `(:deferred-warnings ,deferred-warnings))
        ,@(when lexicals `(:lexicals ,lexicals))))))

;; Scheduling: the main process keeps a pool of up to *MAX-FORKS* background workers, forked
;; copies of itself, that perform file-producing actions such as compiling a Lisp file; meanwhile,
;; the main process performs the in-image actions, such as loading the FASLs compiled so far.
;;
;; A worker sees the state of the main process at the time it was forked. Each in-image action
;; that modifies the image starts a new "generation" of the image, and each action requires the
;; latest generation produced by any of its (transitive) dependencies. A worker can perform any
;; action that requires a generation no later than its own; when no idle worker is recent enough
;; for a ready action, a new worker is forked, replacing an idle one if there are too many already.
;; Reusing workers matters, since forking a large image is expensive.
;;
;; In a deterministic build, in-image actions are performed in exactly the same order as in a
;; sequential plan. A background action that fails is retried in the main process once all the
;; in-image actions planned before it are done, i.e. in the same state as a sequential build would
;; have performed it, so that a system with missing dependencies still builds.
(defmethod perform-plan ((plan parallel-plan) &key verbose &allow-other-keys)
  (unless (can-fork-or-warn)
    ;; Sequential fallback: PLAN-ACTIONS lists the pending actions in sequential order.
    (return-from perform-plan (call-next-method)))
  (let* ((pending (coerce (plan-pending-actions plan) 'vector))
         (total (length pending))
         (deterministic-p (plan-deterministic-p plan))
         (index-of #'(lambda (action) (plan-action-index plan action)))
         (position-of (make-hash-table :test 'equal :size (max 16 total)))
         ;; action -> list of pending actions it depends on
         (dependencies-of (make-hash-table :test 'equal :size (max 16 total)))
         ;; action -> number of pending dependencies not yet performed
         (waiting-counts (make-hash-table :test 'equal :size (max 16 total)))
         ;; action -> list of pending actions that depend on it
         (dependents (make-hash-table :test 'equal :size (max 16 total)))
         ;; finished action -> the image generation that actions depending on it require
         (generations (make-hash-table :test 'equal :size (max 16 total)))
         (image-generation 0)
         ;; The pending ordered in-image actions, in plan order, and how many are known finished.
         (ordered (remove-if-not #'(lambda (action)
                                     (ordered-action-p (action-operation action)
                                                       (action-component action)))
                                 pending))
         (ordered-done 0)
         ;; Ready actions, by kind.
         (bg-ready (make-priority-queue :key index-of))
         (fg-ready (make-priority-queue :key index-of))
         (unordered-ready (simple-queue))
         (retry-ready (make-priority-queue :key index-of))
         (workers '())
         (fork-disabled-p nil)
         (fg-count 0)
         (bg-count 0)
         (retry-count 0)
         (fork-count 0)
         (peak-workers 0)
         (all-deferred-warnings '())
         ;; component -> plist of reified names of things missing when a worker compiled it,
         ;; see NOW-DEFINED-MISSING-DEFINITIONS
         (missing-definitions (make-hash-table :test 'eq))
         (to-go (planned-output-action-count *asdf-session*))
         (ltogo (if (plusp to-go) (ceiling (log (1+ to-go) 10)) 1))
         (start-time (get-internal-real-time))
         (start-times (make-hash-table :test 'equal))
         (timeline '())
         (foreground-time 0)
         (waiting-before *time-spent-waiting*))
    ;; Build the scheduling graph of pending actions.
    (setf (slot-value plan 'starting-points) (simple-queue))
    (let ((cache (make-hash-table :test 'equal)))
      (loop :for action :across pending
            :for position :from 0
            :for dependencies = (action-pending-dependencies plan action cache)
            :do (setf (gethash action position-of) position
                      (gethash action dependencies-of) dependencies
                      (gethash action waiting-counts) (length dependencies))
                (dolist (dependency dependencies)
                  (when (> (funcall index-of dependency) (funcall index-of action))
                    ;; Can't happen with ASDF's traversal, but if it ever does, fall back to
                    ;; plain dataflow order rather than risk a deadlock.
                    (setf deterministic-p nil))
                  (push action (gethash dependency dependents)))
                (when (null dependencies)
                  (enqueue (plan-starting-points plan) action))))
    (labels ((describe-action (action)
               (action-description (action-operation action) (action-component action)))
             (now ()
               (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
             (record (action where start)
               (let ((end (now)))
                 (unless (eq where :background)
                   (incf foreground-time (- end start)))
                 (push (list (describe-action action) where start end) timeline)))
             (ordered-floor ()
               ;; The plan index of the first ordered in-image action not yet performed.
               (loop :while (and (< ordered-done (length ordered))
                                 (gethash (aref ordered ordered-done) generations))
                     :do (incf ordered-done))
               (if (< ordered-done (length ordered))
                   (funcall index-of (aref ordered ordered-done))
                   most-positive-fixnum))
             (required-generation (action)
               (loop :for dependency :in (gethash action dependencies-of)
                     :maximize (gethash dependency generations 0)))
             (make-ready (action)
               (let ((o (action-operation action))
                     (c (action-component action)))
                 (cond
                   ((background-action-p plan action)
                    (priority-queue-push bg-ready action))
                   ((ordered-action-p o c)
                    (priority-queue-push fg-ready action))
                   (t
                    (enqueue unordered-ready action)))))
             (finish (action image-modified-p)
               ;; ACTION was performed: release the actions that were waiting for it.
               (setf (gethash action generations)
                     (if image-modified-p
                         (incf image-generation)
                         (required-generation action)))
               (dolist (dependent (gethash action dependents))
                 (when (zerop (decf (gethash dependent waiting-counts)))
                   (make-ready dependent))))
             (announce (action backgroundp)
               (when verbose
                 (format t "~&Will ~:[try~;skip~] ~A in ~:[foreground~;background~]~%"
                         (action-already-done-p plan (action-operation action)
                                                (action-component action))
                         (describe-action action) backgroundp)))
             (note-output-action-done (action)
               (decf to-go)
               (asdf-message "~&[~vd to go] Done ~A~%" ltogo to-go (describe-action action))
               (finish-outputs))
             (check-missing-definitions (o c)
               ;; Before loading a file compiled by a worker, check that the worker didn't
               ;; miss definitions it used; if it did, compile the file again, here.
               (when (typep o 'asdf:load-op)
                 (let ((missing (now-defined-missing-definitions
                                 (gethash c missing-definitions))))
                   ;; In a deterministic build, definitions loaded later are also missing in a
                   ;; sequential build. Otherwise, keep checking until the end of the build.
                   (when (or missing deterministic-p)
                     (remhash c missing-definitions))
                   (when missing
                     (asdf-message "~&~A was compiled without ~{~A~^, ~}, ~
                                    probably due to a missing dependency. Compiling it again.~%"
                                   c missing)
                     (let ((compile-op (find-operation o 'asdf:compile-op)))
                       (perform-with-restarts compile-op c)
                       (mark-as-done plan compile-op c))
                     (incf retry-count)))))
             (perform-in-foreground (action)
               (let ((o (action-operation action))
                     (c (action-component action))
                     (performedp nil))
                 (announce action nil)
                 (check-missing-definitions o c)
                 ;; It may have been done meanwhile, e.g. by an OPERATE within a PERFORM.
                 (unless (action-already-done-p plan o c)
                   (let ((start (now)))
                     (perform-with-restarts o c)
                     (mark-as-done plan o c)
                     (setf performedp t)
                     (record action :foreground start)))
                 (incf fg-count)
                 (finish action (and performedp (ordered-action-p o c)))))
             (retry-in-foreground (action &optional (retryp t))
               ;; Perform in the main process an action meant for the background.
               (let ((o (action-operation action))
                     (c (action-component action)))
                 (announce action nil)
                 (let ((start (now)))
                   (perform-with-restarts o c)
                   (mark-as-done plan o c)
                   (record action (if retryp :retry :foreground) start))
                 (if retryp (incf retry-count) (incf fg-count))
                 (note-output-action-done action)
                 ;; e.g. compiling a file in the image has compile-time side effects.
                 (finish action t)))
             (assign (worker action)
               (announce action t)
               (setf (gethash action start-times) (now))
               (assign-job worker action (gethash action position-of))
               (incf bg-count))
             (fork-worker ()
               (let ((worker (start-worker pending #'perform-in-background
                                           image-generation workers)))
                 (push worker workers)
                 (incf fork-count)
                 (setf peak-workers (max peak-workers (length workers)))
                 (setf *max-actual-forks* (max *max-actual-forks* (length workers)))
                 worker))
             (remove-worker (worker)
               (setf workers (remove worker workers)))
             (job-finished (worker)
               (let* ((job (worker-job worker))
                      (action (job-item job))
                      (o (action-operation action))
                      (c (action-component action))
                      (condition (job-condition job)))
                 (setf (worker-job worker) nil)
                 (destructuring-bind (&key deferred-warnings lexicals &allow-other-keys)
                     (job-result job)
                   (when deferred-warnings
                     (push deferred-warnings all-deferred-warnings))
                   (when (and (typep o 'asdf:compile-op) (or deferred-warnings lexicals))
                     (setf (gethash c missing-definitions)
                           (list :functions (reified-undefined :function deferred-warnings)
                                 :variables (reified-undefined :variable deferred-warnings)
                                 :lexicals lexicals))))
                 (cond
                   (condition
                    ;; Retry in the main process, so the user gets the usual ASDF error behavior
                    ;; and restarts, and a chance to debug the issue. The failure is often due
                    ;; to a missing dependency, that the retry will work around.
                    (discard-job-output job)
                    (asdf-message "~&Failed ~A in the background: ~A~%Will retry in the foreground.~%"
                                  (describe-action action) condition)
                    (priority-queue-push retry-ready action))
                   (t
                    (record action :background (gethash action start-times))
                    (replay-job-output job)
                    ;; The worker updated its own copy of the operation times; do it here.
                    (mark-operation-done o c)
                    (mark-as-done plan o c)
                    (note-output-action-done action)
                    (finish action nil)))))
             (collect-finished-workers (wait)
               (multiple-value-bind (completed dead) (finished-workers workers :wait wait)
                 (dolist (worker completed)
                   (let ((failedp (job-condition (worker-job worker))))
                     (job-finished worker)
                     (when failedp
                       ;; The worker exits after a failure rather than reuse a dubious image.
                       (retire-worker worker)
                       (remove-worker worker))))
                 (dolist (worker dead)
                   (remove-worker worker)
                   (when (worker-job worker)
                     (job-finished worker)))))
             (start-background-actions ()
               (loop :until (empty-p bg-ready)
                     :do (let* ((action (priority-queue-peek bg-ready))
                                (required (required-generation action))
                                (idle (remove-if-not #'worker-idle-p workers))
                                (worker (find-if #'(lambda (worker)
                                                     (>= (worker-generation worker) required))
                                                 idle)))
                           (cond
                             ((not (background-action-p plan action))
                              ;; Done meanwhile, or POIU was told not to fork any more.
                              (perform-in-foreground (priority-queue-pop bg-ready)))
                             (worker
                              (assign worker (priority-queue-pop bg-ready)))
                             ((build-is-serial-p action)
                              (retry-in-foreground (priority-queue-pop bg-ready) nil))
                             ((not (can-fork-p))
                              ;; Some action started threads, so it's no longer safe to fork.
                              ;; Existing workers remain usable, being separate processes,
                              ;; but actions requiring a new worker are performed here,
                              ;; in plan order, like the actions that fail in the background.
                              (unless fork-disabled-p
                                (setf fork-disabled-p t)
                                (asdf-message "~&POIU: threads were started; ~
                                               performing remaining actions in this process.~%"))
                              (priority-queue-push retry-ready (priority-queue-pop bg-ready)))
                             ((< (length workers) *max-forks*)
                              (assign (fork-worker) (priority-queue-pop bg-ready)))
                             (idle
                              ;; Make room for a more recent worker.
                              (retire-worker (first idle))
                              (remove-worker (first idle)))
                             (t
                              (return))))))
             (foreground-choice ()
               ;; Which queue to take the next foreground action from, if any may run now.
               (let ((floor (ordered-floor))
                     (retry (priority-queue-peek retry-ready))
                     (ordered (priority-queue-peek fg-ready)))
                 (cond
                   ((not (empty-p unordered-ready))
                    :unordered)
                   ((and retry (< (funcall index-of retry) floor))
                    :retry)
                   ((and ordered
                         (or (not deterministic-p)
                             (and (= (funcall index-of ordered) floor)
                                  (or (null retry)
                                      (< (funcall index-of ordered) (funcall index-of retry))))))
                    :ordered))))
             (run-foreground-action ()
               ;; Perform one ready in-image action (or retry), if allowed. Return true if any.
               (ecase (foreground-choice)
                 (:unordered (perform-in-foreground (dequeue unordered-ready)) t)
                 (:retry (retry-in-foreground (priority-queue-pop retry-ready)) t)
                 (:ordered (perform-in-foreground (priority-queue-pop fg-ready)) t)
                 ((nil) nil)))
             (build-is-serial-p (action)
               ;; Is ACTION the only thing that can happen now, in the same state as in a
               ;; sequential build? Then forking a worker for it would only add overhead.
               (and (= (size bg-ready) 1)
                    (notany (complement #'worker-idle-p) workers)
                    (null (foreground-choice))
                    (< (funcall index-of action) (ordered-floor))))
             (unstick ()
               ;; Nothing is running and nothing may run: this can't happen if the plan order is
               ;; consistent with the dependencies, but don't deadlock if it somehow does.
               (cond
                 ((not (empty-p retry-ready))
                  (retry-in-foreground (priority-queue-pop retry-ready))
                  t)
                 ((and deterministic-p (not (empty-p fg-ready)))
                  (warn "POIU: in-image actions became ready out of plan order; ~
                         disabling deterministic ordering")
                  (setf deterministic-p nil)
                  t))))
      (loop :for action :in (dequeue-all (plan-starting-points plan))
            :do (make-ready action))
      (call-with-result-directory
       #'(lambda ()
           (unwind-protect
                (loop
                  ;; Collect results from finished workers, then keep the CPUs busy, then do
                  ;; in-image work while the workers run, else wait for them.
                  (collect-finished-workers nil)
                  (start-background-actions)
                  (cond
                    ((run-foreground-action))
                    ((some (complement #'worker-idle-p) workers)
                     (collect-finished-workers t))
                    ((unstick))
                    (t (return))))
             (retire-workers workers))))
      ;; In a non-deterministic build, a file may have been loaded before the definitions it was
      ;; missing; all we can do then is warn about it.
      (loop :for c :being :the :hash-keys :of missing-definitions
              :using (:hash-value names)
            :for missing = (now-defined-missing-definitions names)
            :when missing
              :do (warn "POIU: ~A was compiled and loaded without ~{~A~^, ~}; ~
                         it probably lacks a dependency. Build it with ~S ~S to avoid this."
                        c missing '*parallel-plan-deterministic-p* t))
      (dolist (deferred-warnings (reverse all-deferred-warnings))
        (unreify-worker-deferred-warnings deferred-warnings))
      (setf *last-build-statistics*
            (list :actions total :foreground fg-count :background bg-count :retried retry-count
                  :max-forks *max-forks* :forks fork-count :peak-workers peak-workers
                  :elapsed (float (now))
                  :foreground-time (float foreground-time)
                  :waiting (float (/ (- *time-spent-waiting* waiting-before)
                                     internal-time-units-per-second)))
            *last-build-timeline* (nreverse timeline))
      (let ((unfinished (loop :for action :across pending
                              :unless (gethash action generations) :collect action)))
        (when unfinished
          (error "POIU: ~D of ~D actions could not be performed, ~
                  as they were still waiting for their dependencies:~%~{  ~A~%~}"
                 (length unfinished) total (mapcar #'describe-action unfinished)))))))

;;; Breadcrumbs: feature to replay otherwise non-deterministic builds
(defvar *breadcrumb-stream* nil
  "Stream that records the trail of operations on components.
As the order of ASDF operations in general and parallel operations in
particular are randomized, it is necessary to record them to replay &
debug them later.")
(defvar *breadcrumbs* nil
  "Actual breadcrumbs found, to override traversal for replay and debugging")

(defmethod perform :after (operation component)
  "Record the operations and components in a stream of breadcrumbs."
  (when *breadcrumb-stream*
    (format *breadcrumb-stream* "~S~%" (action-path (cons operation component)))
    (force-output *breadcrumb-stream*)))

(defun read-breadcrumbs-from (operation pathname)
  (with-open-file (f pathname)
    (loop :for (op . comp) = (read f nil nil) :while op
          :collect (cons (find-operation operation op) (find-component () comp)))))

(defun call-recording-breadcrumbs (pathname record-p thunk)
  (if (and record-p (not *breadcrumb-stream*))
      (let ((*breadcrumb-stream*
              (progn
                (delete-file-if-exists pathname)
                (open pathname :direction :output
                               :if-exists :overwrite
                               :if-does-not-exist :create))))
        (format *breadcrumb-stream* ";; Breadcrumbs~%")
        (unwind-protect
             (funcall thunk)
          (close *breadcrumb-stream*)))
      (funcall thunk)))

(defmacro recording-breadcrumbs ((pathname record-p) &body body)
  `(call-recording-breadcrumbs ,pathname ,record-p (lambda () ,@body)))

(defmethod operate :before ((operation operation) (component t) &key
                            (breadcrumbs-to nil record-breadcrumbs-p)
                            ((:using-breadcrumbs-from breadcrumb-input-pathname)
                             (make-broadcast-stream) read-breadcrumbs-p)
                            &allow-other-keys)
  (recording-breadcrumbs (breadcrumbs-to record-breadcrumbs-p)
    (when read-breadcrumbs-p
      (perform-plan (read-breadcrumbs-from operation breadcrumb-input-pathname)))))

(setf *plan-class* 'parallel-plan)
