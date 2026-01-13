(uiop:define-package :poiu
  (:use :uiop/common-lisp :uiop
        :poiu/queue :poiu/fork :poiu/background-process
        :asdf/session :asdf/upgrade
        :asdf/component :asdf/system :asdf/find-component :asdf/operation
        :asdf/action :asdf/plan :asdf/operate
        :asdf/output-translations)
  (:use-reexport :poiu/action-graph))

(in-package :poiu)

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
    (let* ((all-deferred-warnings nil)
           (planned-output-action-count (planned-output-action-count *asdf-session*))
           (ltogo (unless (zerop planned-output-action-count) (ceiling (log planned-output-action-count 10))))
           (fg-queue (simple-queue))
           (bg-queue (simple-queue)))
      (labels ((background-p (action)
                 (destructuring-bind (o . c) action
                   (and
                    ;; Must be safe to parallelize, not be required in the image, & not already be done
                    ;;(poiu-parallelizable-action-p plan o c)
                    (not (needed-in-image-p o c))
                    (not (action-already-done-p plan o c)))))
               (categorize-starting-points ()
                 (loop :for action :in (dequeue-all starting-points) :do
                   (enqueue (if (background-p action) bg-queue fg-queue) action))))
        (categorize-starting-points)
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
               (cond
                 (condition
                  (finish-outputs)
                  (warn "Failed ~A~:[~; in the background~]. Retrying~:*~:[~; in the foreground~]."
                        (action-description o c) backgroundp)
                  (finish-outputs)
                  (perform-with-restarts o c))
                 (t nil))
               (when backgroundp
                 (decf planned-output-action-count)
                 (asdf-message "~&[~vd to go] Done ~A~%"
                               ltogo planned-output-action-count (action-description o c))
                 (finish-outputs))
               (mark-as-done plan o c)
               (categorize-starting-points)))
          ;; What we do in each forked process
          (destructuring-bind (o . c) action
            (cond
              (backgroundp
               (perform-with-restarts o c)
               `(:deferred-warnings ,(reify-deferred-warnings)))
              ((action-already-done-p plan o c)
               (unless (or (not (needed-in-image-p o c))
                           (action-already-done-p nil o c))
                 (warn "WTF? aedp ~A" (action-description o c)))
               nil)
              (t
               (perform-with-restarts o c)
               nil))))
        (map () #'unreify-deferred-warnings all-deferred-warnings)
        (assert (and (empty-p fg-queue) (empty-p bg-queue) (empty-p children))
                (parents children)
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
         (*plan-class*
           (if (and system-name
                    (not (member system-name '("asdf" "uiop")
                                 :test #'string=)))
               'parallel-plan
               *plan-class*)))
    (apply #'call-next-method operation component args)))

(setf *plan-class* 'parallel-plan)

