(uiop:define-package :poiu
  (:use :uiop/common-lisp :uiop
        :poiu/queue :poiu/fork :poiu/background-process
        :asdf/session :asdf/upgrade
        :asdf/component :asdf/system :asdf/find-component :asdf/operation
        :asdf/action :asdf/plan :asdf/operate
        :asdf/output-translations)
  (:use-reexport :poiu/action-graph))

(in-package :poiu)

(defun maybe-reify-deferred-warnings ()
  (ignore-errors (reify-deferred-warnings)))

(defun package-definition-component-p (component)
  (and (typep component 'asdf:cl-source-file)
       (let* ((component-name (string-downcase (or (component-name component) "")))
              (pathname-name (string-downcase (or (pathname-name (component-pathname component)) ""))))
         (or (member component-name '("package" "packages" "pkgdcl")
                     :test #'string=)
             (member pathname-name '("package" "packages" "pkgdcl")
                     :test #'string=)))))

(defun form-named-p (form name)
  (and (consp form)
       (symbolp (car form))
       (string-equal (symbol-name (car form)) name)))

(defun package-designator-name (designator)
  (etypecase designator
    (string designator)
    (symbol (string designator))))

(defstruct source-phase-info
  complete-p
  in-package-name
  defines-package-p
  foreground-compile-p
  required-packages)

(defvar *source-phase-info-cache* (make-hash-table :test #'equal))

(defun package-option-name (option)
  (and (consp option)
       (symbolp (first option))
       (string-upcase (symbol-name (first option)))))

(defun package-option-required-packages (option)
  (let ((option-name (package-option-name option)))
    (cond
      ((member option-name
               '("USE" "MIX" "REEXPORT" "MIX-REEXPORT" "USE-REEXPORT" "RECYCLE")
               :test #'string=)
       (mapcar #'package-designator-name (rest option)))
      ((member option-name '("IMPORT-FROM" "SHADOWING-IMPORT-FROM") :test #'string=)
       (when (second option)
         (list (package-designator-name (second option)))))
      ((string= option-name "LOCAL-NICKNAMES")
       (loop :for entry :in (rest option)
             :when (and (consp entry) (second entry))
               :collect (package-designator-name (second entry))))
      (t
       nil))))

(defun package-definition-form-p (form)
  (or (form-named-p form "DEFPACKAGE")
      (form-named-p form "DEFINE-PACKAGE")))

(defun compile-time-definition-form-p (form)
  (or (form-named-p form "DEFMACRO")
      (form-named-p form "DEFINE-COMPILER-MACRO")
      (form-named-p form "DEFINE-MODIFY-MACRO")
      (form-named-p form "DEFTYPE")
      (form-named-p form "DEFSTRUCT")
      (form-named-p form "DEFCLASS")
      (form-named-p form "DEFINE-CONDITION")
      (form-named-p form "DEFGENERIC")
      (form-named-p form "DEFMETHOD")))

(defun source-file-uses-reader-eval-p (pathname)
  "Return true when PATHNAME contains read-time evaluation syntax.

This is a conservative text scan: false positives are acceptable because they
only keep a file in the foreground, while false negatives can let worker
processes compile a file without the compile-time state established in the
parent image."
  (with-open-file (stream pathname :direction :input)
    (loop :for line = (read-line stream nil nil)
          :while line
          :thereis (search "#." line))))

(defun compute-source-phase-info (pathname)
  (let ((in-package-name nil)
        (defines-package-p nil)
        (foreground-compile-p
          (and pathname
               (ignore-errors
                 (source-file-uses-reader-eval-p pathname))))
        (required-packages '()))
    (labels ((record-required-packages (packages)
               (dolist (package packages)
                 (pushnew package required-packages :test #'string=)))
             (walk (form)
               (cond
                 ((form-named-p form "IN-PACKAGE")
                  (let ((arguments (cdr form)))
                    (when (and arguments (null in-package-name))
                      (setf in-package-name
                            (package-designator-name (first arguments))))))
                 ((package-definition-form-p form)
                  (setf defines-package-p t)
                  (dolist (option (cddr form))
                    (record-required-packages
                     (package-option-required-packages option))))
                 ((compile-time-definition-form-p form)
                  (setf foreground-compile-p t))
                 ((form-named-p form "EVAL-WHEN")
                  (map () #'walk (cddr form))))))
      (handler-case
          (progn
            (with-open-file (stream pathname :direction :input)
              (loop :for form = (read stream nil nil)
                    :while form
                    :do (walk form)))
            (make-source-phase-info
             :complete-p t
             :in-package-name in-package-name
             :defines-package-p defines-package-p
             :foreground-compile-p foreground-compile-p
             :required-packages (nreverse required-packages)))
        (error ()
          (make-source-phase-info
           :complete-p nil
           :in-package-name in-package-name
           :defines-package-p defines-package-p
           :foreground-compile-p foreground-compile-p
           :required-packages (nreverse required-packages)))))))

(defun analyze-source-file-phase-info (pathname)
  (let ((cached-info (gethash pathname *source-phase-info-cache*)))
    (if (and cached-info (source-phase-info-complete-p cached-info))
        cached-info
        (let ((fresh-info (compute-source-phase-info pathname)))
          (when (source-phase-info-complete-p fresh-info)
            (setf (gethash pathname *source-phase-info-cache*) fresh-info))
          fresh-info))))

(defun source-file-phase-info (pathname)
  (and pathname (analyze-source-file-phase-info pathname)))

(defun source-file-in-package-name (pathname)
  (source-phase-info-in-package-name (source-file-phase-info pathname)))

(defun source-file-defines-package-p (pathname)
  (source-phase-info-defines-package-p (source-file-phase-info pathname)))

(defun source-file-foreground-compile-p (pathname)
  (source-phase-info-foreground-compile-p (source-file-phase-info pathname)))

(defun source-file-analysis-complete-p (pathname)
  (source-phase-info-complete-p (source-file-phase-info pathname)))

(defun source-file-package-ready-p (component)
  (let* ((pathname (component-pathname component))
         (phase-info (source-file-phase-info pathname))
         (package-name (and phase-info (source-phase-info-in-package-name phase-info))))
    (or (null pathname)
        (and phase-info
             (or (null package-name)
                 (find-package package-name))))))

(defun source-file-required-packages-ready-p (component)
  (let* ((pathname (component-pathname component))
         (phase-info (source-file-phase-info pathname)))
    (or (null pathname)
        (and phase-info
             (every #'find-package (source-phase-info-required-packages phase-info))))))

(defun source-file-phase-ready-p (component)
  (and (source-file-package-ready-p component)
       (source-file-required-packages-ready-p component)))

(defun pending-build-action-p (plan action)
  (destructuring-bind (o . c) action
    (let ((status (action-status plan o c)))
      (and status
           (status-need-p status)
           (not (action-already-done-p plan o c))))))

(defun build-phase-barrier-action-p (plan action)
  (destructuring-bind (o . c) action
    (and (pending-build-action-p plan action)
         (needed-in-image-p o c)
         ;; PREPARE-OP encodes prerequisites for later image actions, but it
         ;; is not itself a useful phase boundary for background compilation.
         (not (typep o 'asdf:prepare-op)))))

(defun build-phase-action-ready-p (plan action)
  (empty-p (action-map (plan-children plan) action)))

(defun next-ready-build-phase-barrier-index (plan actions)
  (loop :with earliest = nil
        :for action :in actions
        :when (and (build-phase-barrier-action-p plan action)
                   (build-phase-action-ready-p plan action))
          :do (let ((action-index (plan-action-index plan action)))
                (setf earliest
                      (if earliest
                          (min earliest action-index)
                          action-index)))
        :finally (return earliest)))

(defstruct (build-phase-state (:constructor make-build-phase-state ()))
  (deferred-actions (simple-queue))
  (forced-actions (make-hash-table :test #'equal))
  barrier-index)

(defun source-compile-action-p (action)
  (destructuring-bind (o . c) action
    (and (typep o 'asdf:compile-op)
         (typep c 'asdf:cl-source-file))))

(defun build-phase-ready-actions (starting-points fg-queue)
  (remove-duplicates
   (append (queue-contents starting-points)
           (queue-contents fg-queue))
   :test #'equal))

(defun refresh-build-phase-state (plan state starting-points fg-queue)
  (setf (build-phase-state-barrier-index state)
        (next-ready-build-phase-barrier-index
         plan
         (build-phase-ready-actions starting-points fg-queue))))

(defun action-before-build-phase-barrier-p (plan state action)
  (let ((barrier-index (build-phase-state-barrier-index state)))
    (or (null barrier-index)
        (< (plan-action-index plan action) barrier-index))))

(defun action-ready-in-current-build-phase-p (plan state action)
  (destructuring-bind (o . c) action
    (or (not (source-compile-action-p action))
        (and (or (needed-in-image-p o c)
                 (action-before-build-phase-barrier-p plan state action))
             (source-file-phase-ready-p c)))))

(defun defer-build-phase-action (state action)
  (enqueue (build-phase-state-deferred-actions state) action))

(defun release-deferred-build-phase-actions (state starting-points)
  (loop :for action :in (dequeue-all (build-phase-state-deferred-actions state))
        :do (enqueue starting-points action)))

(defun build-phase-state-empty-p (state)
  (empty-p (build-phase-state-deferred-actions state)))

(defun build-phase-state-idle-p (state)
  (and (build-phase-state-empty-p state)
       (zerop (hash-table-count (build-phase-state-forced-actions state)))))

(defun note-forced-build-phase-action (state action)
  (setf (gethash action (build-phase-state-forced-actions state)) t))

(defun consume-forced-build-phase-action-p (state action)
  (prog1 (gethash action (build-phase-state-forced-actions state))
    (remhash action (build-phase-state-forced-actions state))))

(defun force-deferred-build-phase-action (plan state fg-queue deterministicp)
  (let ((deferred-actions (dequeue-all (build-phase-state-deferred-actions state))))
    (when deferred-actions
      (let* ((ordered-actions
               (if deterministicp
                   (sort deferred-actions #'< :key #'(lambda (action)
                                                      (plan-action-index plan action)))
                   deferred-actions))
             (forced-action (first ordered-actions)))
        (dolist (blocked-action (rest ordered-actions))
          (defer-build-phase-action state blocked-action))
        (note-forced-build-phase-action state forced-action)
        (enqueue fg-queue forced-action)
        t))))

(defun component-in-serial-build-segment-p (component)
  (let* ((parent (component-parent component))
         (siblings (and parent (component-children parent)))
         (position (and siblings (position component siblings))))
    (and position
         (let* ((name (component-name component))
                (previous (and (plusp position)
                               (nth (1- position) siblings)))
                (next (and (< position (1- (length siblings)))
                           (nth (1+ position) siblings))))
           (or (and previous
                    (member (component-name previous)
                            (component-sideway-dependencies component)
                            :test #'string=))
               (and next
                    (member name
                            (component-sideway-dependencies next)
                            :test #'string=)))))))

(defun source-file-requires-foreground-compile-p (component)
  (and (typep component 'asdf:cl-source-file)
       (let ((pathname (component-pathname component)))
         (or (package-definition-component-p component)
             (not (source-file-analysis-complete-p pathname))
             (source-file-defines-package-p pathname)
             (source-file-foreground-compile-p pathname)
             (component-in-serial-build-segment-p component)))))

(defun action-runnable-in-background-p (plan action)
  (destructuring-bind (o . c) action
    (and (not (source-file-requires-foreground-compile-p c))
         (not (needed-in-image-p o c))
         (not (action-already-done-p plan o c)))))

(defun define-op-p (operation)
  (let ((class (find-class (find-symbol "DEFINE-OP" "ASDF/FIND-SYSTEM") nil)))
    (and class (typep operation class))))

(defun action-output-stamp (operation component)
  (let* ((outputs (remove-if 'null (output-files operation component)))
         (stamps (mapcar #'get-file-stamp outputs)))
    (and outputs
         (notany #'null stamps)
         (timestamps-earliest stamps))))

(defun mark-background-action-as-done (plan operation component)
  (let* ((plan-status (action-status plan operation component))
         (stamp (action-output-stamp operation component)))
    (assert stamp ()
            "Background action ~A completed but produced no observable output stamp"
            (action-description operation component))
    (setf (component-operation-time operation component) stamp
          (action-status plan operation component)
          (asdf/plan::make-action-status
           :bits (logior (asdf/plan::status-bits plan-status) asdf/plan::+done-bit+)
           :stamp stamp
           :level (asdf/plan::status-level plan-status)
           :index (status-index plan-status))))
  (poiu/action-graph::parallel-plan-mark-as-done plan operation component))

(defun collapse-stalled-done-actions (plan)
  (let ((count 0))
    (dolist (action (remove-duplicates
                     (append (action-map-keys (plan-parents plan))
                             (action-map-keys (plan-children plan)))
                     :test #'equal))
      (destructuring-bind (o . c) action
        (when (action-already-done-p plan o c)
          (incf count)
          (poiu/action-graph::parallel-plan-mark-as-done plan o c))))
    count))

(defun condition-message-string (condition)
  (typecase condition
    (poiu/background-process::process-failed
     (or (poiu/background-process::process-failed-condition-message condition)
         (ignore-errors
           (princ-to-string condition))))
    (condition
     (ignore-errors
       (princ-to-string condition)))
    (t
     nil)))

(defun build-phase-deferable-condition-p (condition)
  (let ((message (condition-message-string condition)))
    (or (typep condition 'package-error)
        (typep condition 'unbound-variable)
        (and message
             (or (and (search "package " message :test #'char-equal)
                      (search "does not exist" message :test #'char-equal))
                 (search "does not designate any package" message :test #'char-equal)
                 (search " is unbound" message :test #'char-equal)
                 (search " is undefined" message :test #'char-equal))))))

(defun phase-deferable-condition-p (condition)
  (build-phase-deferable-condition-p condition))

;;; Performing a parallel plan
(defun action-result-file (o c)
  (let ((p (component-pathname c)))
    (apply-output-translations
     (make-pathname :name (format nil "~A.ASDF-~A" (file-namestring p) (type-of o))
                    :type "process-result" :defaults p))))

;; (defun poiu-parallelizable-action-p (plan operation component)
;;   "Return true iff it is safe to execute OPERATION on COMPONENT in parallel.

;; This conservative predicate intentionally only allows file-level COMPILE-OPs.
;; All other actions must remain sequential to preserve ASDF invariants."
;;   (declare (ignore plan))
;;   (and
;;    ;; Only compile operations
;;    (typep operation 'asdf:compile-op)

;;    ;; Only source files (no systems, no modules)
;;    (typep component 'asdf:cl-source-file)

;;    ;; Never parallelize ASDF or UIOP themselves
;;    (let ((system (asdf:component-system component)))
;;      (not (member (asdf:component-name system)
;;                   '("asdf" "uiop")
;;                   :test #'string=)))))

(defmethod perform-plan ((plan parallel-plan) &key verbose &allow-other-keys)

  ;; (when (some (lambda (action)
  ;;               (destructuring-bind (o . c) action
  ;;                 (let ((system (asdf:component-system c)))
  ;;                   (and system
  ;;                        (member (asdf:component-name system)
  ;;                                '("asdf" "uiop")
  ;;                                :test #'string=)))))
  ;;             (plan-actions plan))
  ;;   (return-from perform-plan (call-next-method)))

  (unless (can-fork-or-warn)
    (return-from perform-plan (call-next-method)))
  (with-slots (starting-points children parents) plan
    (dequeue-all starting-points)
    (dolist (action (plan-actions plan))
      (destructuring-bind (o . c) action
        (let ((status (action-status plan o c)))
          (when (and status
                     (status-need-p status)
                     (empty-p (action-map children action)))
            (enqueue starting-points action)))))
    (let* ((all-deferred-warnings nil)
           (planned-output-action-count (planned-output-action-count *asdf-session*))
           (ltogo (unless (zerop planned-output-action-count) (ceiling (log planned-output-action-count 10))))
           (fg-queue (simple-queue))
           (bg-queue (simple-queue))
           (phase-queue (simple-queue))
           (forced-phase-actions (make-hash-table :test #'equal))
           (build-phase-barrier-index nil))
      (labels ((action-phase-ready-p (action)
                 (destructuring-bind (o . c) action
                   (or (not (and (typep o 'asdf:compile-op)
                                 (typep c 'asdf:cl-source-file)))
                       (and (or (needed-in-image-p o c)
                                (null build-phase-barrier-index)
                                (< (plan-action-index plan action)
                                   build-phase-barrier-index))
                            (source-file-phase-ready-p c)))))
               (refresh-build-phase-barrier ()
                 (setf build-phase-barrier-index
                       (next-ready-build-phase-barrier-index
                        plan
                        (remove-duplicates
                         (append (queue-contents starting-points)
                                 (queue-contents fg-queue))
                         :test #'equal))))
               (background-p (action)
                 (destructuring-bind (o . c) action
                   (and
                    ;; Must be safe to parallelize, not be required in the image, & not already be done
                    ;;(poiu-parallelizable-action-p plan o c)
                    (not (package-definition-component-p c))
                    (not (and (typep c 'asdf:cl-source-file)
                              (or (not (source-file-analysis-complete-p (component-pathname c)))
                                  (source-file-defines-package-p (component-pathname c))
                                  (source-file-foreground-compile-p (component-pathname c))
                                  (component-in-serial-build-segment-p c))))
                    (not (needed-in-image-p o c))
                    (not (action-already-done-p plan o c)))))
               (defer-phase-action (action)
                 (enqueue phase-queue action))
               (release-phase-actions ()
                 (refresh-build-phase-barrier)
                 (loop :for action :in (dequeue-all phase-queue) :do
                   (enqueue starting-points action))
                 (categorize-starting-points))
               (force-phase-action ()
                 (let ((phase-actions (dequeue-all phase-queue)))
                   (when phase-actions
                     (let* ((ordered-actions
                              (if (plan-deterministic-p plan)
                                  (sort phase-actions #'< :key #'(lambda (action)
                                                                  (plan-action-index plan action)))
                                  phase-actions))
                            (forced-action (first ordered-actions)))
                       (dolist (blocked-action (rest ordered-actions))
                         (defer-phase-action blocked-action))
                       (setf (gethash forced-action forced-phase-actions) t)
                       (enqueue fg-queue forced-action)
                       t))))
               (categorize-starting-points ()
                 (refresh-build-phase-barrier)
                 (loop :for action :in (dequeue-all starting-points) :do
                   (if (action-phase-ready-p action)
                       (enqueue (if (background-p action) bg-queue fg-queue) action)
                       (defer-phase-action action)))))
        (categorize-starting-points)
        (loop
          (doqueue/forking
              (fg-queue bg-queue
               :variables (:item action :backgroundp backgroundp :result result :condition condition)
               :deterministic-order
               (when (plan-deterministic-p plan)
                 #'(lambda (action)
                     (plan-action-index plan action)))
               :announce
               (when verbose
                 (destructuring-bind (o . c) action
                   (format t "~&Will ~:[try~;skip~] ~A in ~:[foreground~;background~]~%"
                           (action-already-done-p plan o c)
                           (action-description o c) backgroundp)))
               :result-file
               (destructuring-bind (o . c) action (action-result-file o c))
               ;; How we cleanup in the foreground after an action is run
               :cleanup
               (destructuring-bind (o . c) action
                 (destructuring-bind (&key deferred-warnings &allow-other-keys) result
                   (when deferred-warnings
                     (push deferred-warnings all-deferred-warnings)))
                 (let ((completedp nil)
                       (forcedp (gethash action forced-phase-actions)))
                   (remhash action forced-phase-actions)
                   (cond
                     ((and condition
                           (phase-deferable-condition-p condition)
                           (not forcedp))
                      (defer-phase-action action))
                     (condition
                      (finish-outputs)
                      (warn "Failed ~A~:[~; in the background~]. Retrying~:*~:[~; in the foreground~]."
                            (action-description o c) backgroundp)
                      (finish-outputs)
                      (perform-with-restarts o c)
                      (setf completedp t))
                     (t
                      (setf completedp t)))
                   (when completedp
                     (when backgroundp
                       (decf planned-output-action-count)
                       (asdf-message "~&[~vd to go] Done ~A~%"
                                     ltogo planned-output-action-count (action-description o c))
                       (finish-outputs))
                     (if backgroundp
                         (mark-background-action-as-done plan o c)
                         (mark-as-done plan o c))
                     (categorize-starting-points)
                     (release-phase-actions)))))
            ;; What we do in each forked process
            (destructuring-bind (o . c) action
              (cond
                (backgroundp
                 (perform-with-restarts o c)
                 (let ((deferred-warnings (maybe-reify-deferred-warnings)))
                   (when deferred-warnings
                     `(:deferred-warnings ,deferred-warnings))))
                ((action-already-done-p plan o c)
                 nil)
                (t
                 (perform-with-restarts o c)
                 nil))))
          (cond
            ((empty-p children)
             (if (empty-p phase-queue)
                 (return)
                 (progn
                   (release-phase-actions)
                   (unless (or (not (empty-p fg-queue))
                               (not (empty-p bg-queue))
                               (force-phase-action))
                     (return)))))
            ((plusp (collapse-stalled-done-actions plan))
             (release-phase-actions))
            ((force-phase-action)
             nil)
            (t
             (return))))
        (map () #'unreify-deferred-warnings all-deferred-warnings)
        (assert (and (empty-p fg-queue) (empty-p bg-queue) (empty-p phase-queue) (empty-p children))
                (parents children phase-queue)
                "Problem with the dependency graph: ~A"
                (summarize-plan plan))))))

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

(defmethod asdf/operate:operate :around
    ((operation asdf:operation) (component asdf:component)
     &rest args &key &allow-other-keys)
  (let* ((system (asdf:component-system component))
         (system-name (and system (asdf:component-name system)))
         (recursivep (consp (visiting-action-list *asdf-session*)))
         (*plan-class*
           (if (and (not recursivep)
                    (not (define-op-p operation))
                    system-name
                    (not (member system-name '("asdf" "uiop")
                                 :test #'string=)))
               'parallel-plan
               'sequential-plan)))
    (apply #'call-next-method operation component args)))

(setf *plan-class* 'parallel-plan)
