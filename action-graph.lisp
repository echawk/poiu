(uiop:define-package :poiu/action-graph
  (:use :uiop/common-lisp :uiop :poiu/queue
        :asdf/upgrade :asdf/session
        :asdf/component :asdf/system :asdf/find-system :asdf/find-component
        :asdf/operation :asdf/action :asdf/plan)
  (:export #:parallel-plan #:*parallel-plan-deterministic-p*
           #:summarize-plan #:serialize-plan #:check-invariants
           #:starting-points #:children #:parents ;; slot names -- FIXME, have clients use accessors
           #:plan-starting-points #:plan-children #:plan-parents #:plan-action-index
           #:plan-deterministic-p #:plan-pending-actions #:pending-action-p
           #:action-pending-dependencies
           #:action-map #:action-map-keys #:action-map-values))
(in-package :poiu/action-graph)

(defvar *parallel-plan-deterministic-p* t
  "When true (the default), in-image actions such as loading FASLs are performed in
exactly the order a sequential ASDF plan would perform them; only file-producing
actions such as compilation are reordered and run in parallel.")

(defclass parallel-plan (plan-traversal)
  ((starting-points
    :initform (simple-queue) :reader plan-starting-points
    :documentation "a queue of pending actions with no pending dependencies,
as computed by PLAN-PENDING-ACTIONS")
   (children
    :initform (make-hash-table :test #'equal) :reader plan-children
    :documentation "map an action to a (hash)set of \"children\" that it depends on")
   (parents
    :initform (make-hash-table :test #'equal) :reader plan-parents
    :documentation "map an action to a (hash)set of \"parents\" that depend on it")
   (all-actions
    :initform (make-array '(0) :adjustable t :fill-pointer 0) :reader plan-all-actions
    :documentation "every action given a status in this plan, in the order statuses were first
assigned, which, as in a SEQUENTIAL-PLAN, places each action after its dependencies")
   (action-indices
    :initform (make-hash-table :test #'equal) :reader plan-action-indices
    :documentation "inverting the all-actions table")
   (visiting-base
    :initform (and *asdf-session* (visiting-action-list *asdf-session*))
    :reader plan-visiting-base
    :documentation "the actions being visited when this plan was created, which belong to an
enclosing plan (if any) rather than to this one")
   (deterministic-p
    :initform *parallel-plan-deterministic-p* :initarg :deterministic-p
    :type boolean :reader plan-deterministic-p
    :documentation "is this plan supposed to be executed in deterministic way?")))

(defgeneric plan-action-index (plan action))
(defmethod plan-action-index ((plan parallel-plan) action)
  (gethash action (plan-action-indices plan)))

(defun ensure-plan-action-index (plan action)
  (or (plan-action-index plan action)
      (setf (gethash action (plan-action-indices plan))
            (vector-push-extend action (plan-all-actions plan)))))

#| ;; We can't do that if we want to trace action-already-done-p
(defmethod print-object ((plan parallel-plan) stream)
  (print-unreadable-object (plan stream :type t :identity t)
    (with-safe-io-syntax (:package :asdf)
      (pprint (summarize-plan plan) stream))))
|#

(defun make-action-map ()
  (make-hash-table :test 'equal))
(defun action-map (map action)
  (gethash action map))
(defun action-unmap (map action)
  (remhash action map))
(defun (setf action-map) (value map action)
  (setf (gethash action map) value))
(defun action-map-values (map)
  (table-values map))
(defun action-map-keys (map)
  (table-keys map))

(defun record-action-dependency (parent child parents children)
  (unless (action-map parents child)
    (setf (action-map parents child) (make-action-map)))
  (when parent
    (unless (action-map children parent)
      (setf (action-map children parent) (make-action-map)))
    (setf (action-map (action-map children parent) child) t)
    (setf (action-map (action-map parents child) parent) t)))

;; TRAVERSE-ACTION calls RECORD-DEPENDENCY on an action *before* visiting it (and thus before
;; the action has a status), every time the action is reached, while the parent that depends on
;; it is the action currently being visited. We therefore record every edge here, and only decide
;; at PERFORM-PLAN time, once all statuses are known, which recorded actions actually need doing.
(defmethod record-dependency ((plan parallel-plan) (o operation) (c component))
  (let* ((action (make-action o c))
         (visiting (visiting-action-list *asdf-session*))
         ;; When this plan was made by an OPERATE nested inside another plan's traversal or
         ;; PERFORM, the action being visited belongs to the enclosing plan, not to this one.
         (parent (unless (eq visiting (plan-visiting-base plan))
                   (first visiting))))
    (record-action-dependency parent action (plan-parents plan) (plan-children plan))))

(defmethod (setf action-status) :after
    (new-status (plan parallel-plan) (o operation) (c component))
  (ensure-plan-action-index plan (make-action o c)))

(defun pending-action-p (plan action)
  "Does ACTION still need to be performed as part of PLAN?"
  (let ((status (action-status plan (action-operation action) (action-component action))))
    (and status (status-need-p status) (not (status-done-p status)))))

(defun plan-pending-actions (plan)
  "The actions that still need to be performed, in sequential plan order."
  (loop :for action :across (plan-all-actions plan)
        :when (pending-action-p plan action) :collect action))

(defun action-pending-dependencies (plan action &optional (cache (make-hash-table :test 'equal)))
  "The pending actions that ACTION must wait for. Dependencies that need not be performed are
looked through, so that their own pending dependencies, if any, still order ACTION: this
preserves every ordering constraint that a sequential plan would honor. CACHE memoizes the
pending frontier of such skipped actions across calls."
  (labels ((add-children (action set)
             (let ((children (action-map (plan-children plan) action)))
               (when children (loop :for child :being :the :hash-keys :of children
                     :do (cond
                           ((pending-action-p plan child)
                            (setf (action-map set child) t))
                           ;; A done action needs nothing more; an up-to-date one won't be
                           ;; performed, but its pending dependencies may matter to its parents.
                           ((not (action-already-done-p plan (action-operation child)
                                                        (action-component child)))
                            (dolist (grandchild (frontier child))
                              (setf (action-map set grandchild) t))))))))
           (frontier (action)
             (multiple-value-bind (frontier foundp) (gethash action cache)
               (if foundp
                   frontier
                   (let ((set (make-action-map)))
                     ;; Mark as in progress, to cut any cycle through actions not performed.
                     (setf (gethash action cache) nil)
                     (add-children action set)
                     (setf (gethash action cache) (action-map-keys set)))))))
    (let ((result (make-action-map)))
      (add-children action result)
      (remhash action result)
      (action-map-keys result))))

(defun summarize-plan (plan)
  (with-slots (starting-points children) plan
    `((:starting-points
       ,(mapcar 'action-path (queue-contents starting-points)))
      (:dependencies
       ,(mapcar #'rest
                (sort
                 (loop :for parent-node :being :the :hash-keys :in children
                       :using (:hash-value progeny)
                       :for parent = parent-node
                       :for (o . c) = parent
                       :collect `(,(or (plan-action-index plan parent) -1)
                                  ,(action-path parent)
                                  ,(if (action-already-done-p plan o c) :- :+)
                                  ,@(loop :for child-node :being :the :hash-keys :in progeny
                                          :using (:hash-value v)
                                          :for child = child-node
                                          :when v :collect (action-path child))))
                 #'< :key #'first))))))

(defgeneric serialize-plan (plan)
  (:documentation
   "Return a sequential list of the actions remaining to perform in PLAN."))
(defmethod serialize-plan ((plan list)) plan)
(defmethod serialize-plan ((plan parallel-plan))
  (plan-pending-actions plan))

(defgeneric check-invariants (object))

(defmethod check-invariants ((plan parallel-plan))
  "Check that the pending actions of PLAN form a DAG consistent with the plan order,
i.e. every pending action comes after all its pending dependencies. Return the pending actions."
  (let ((cache (make-hash-table :test 'equal))
        (pending (plan-pending-actions plan)))
    (dolist (action pending pending)
      (dolist (dependency (action-pending-dependencies plan action cache))
        (unless (< (plan-action-index plan dependency) (plan-action-index plan action))
          (error "Action ~A is planned before its dependency ~A"
                 (action-path action) (action-path dependency)))))))

(defmethod plan-actions ((plan parallel-plan))
  (plan-pending-actions plan))
