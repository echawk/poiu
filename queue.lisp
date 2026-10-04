(uiop/package:define-package :poiu/queue
  (:recycle :poiu/queue :asdf)
  (:use :uiop/common-lisp :uiop/utility)
  (:export
   #:empty-p #:size #:table-keys #:table-values
   #:enqueue #:enqueue-in-front #:dequeue #:queue-contents #:dequeue-all
   #:simple-queue #:with-queue
   #:priority-queue #:make-priority-queue #:priority-queue-push #:priority-queue-pop
   #:priority-queue-peek))
(in-package :poiu/queue)

(with-upgradability ()
;; Some general purpose functions on data structures
(defgeneric empty-p (collection)
  (:documentation "a boolean, true if only the COLLECTION is empty."))
(defgeneric size (collection)
  (:documentation "an integer, the size of the COLLECTION."))

(defgeneric table-keys (table)
  (:documentation "the list of keys in the TABLE"))
(defgeneric table-values (table)
  (:documentation "the list of values in the TABLE"))


;; Our general purpose functions when applied to hash-tables
(defmethod table-values ((table hash-table))
  (loop :for val :being :the :hash-values :of table :collect val))
(defmethod table-keys ((table hash-table))
  (loop :for key :being :the :hash-keys :of table :collect key))
(defmethod size ((table hash-table))
  (hash-table-count table))
(defmethod empty-p ((table hash-table))
  (zerop (hash-table-count table)))

;; Our general purpose functions when applied to NIL
(defmethod empty-p ((x null))
  (declare (ignorable x))
  t)

;; Generic interface to Queue data structures

(defgeneric enqueue (queue value)
  (:documentation "queues the VALUE at the end of the QUEUE"))
(defgeneric enqueue-in-front (queue value)
  (:documentation "queues the VALUE at the beginning of the QUEUE"))
(defgeneric dequeue (queue)
  (:documentation "dequeues the VALUE at the beginning of the QUEUE"))
(defgeneric queue-contents (queue)
  (:documentation "the contents of the QUEUE in dequeueing order from beginning to end.
NB: a copy is made if necessary so that side-effecting the queue won't affect this list."))
(defgeneric dequeue-all (queue)
  (:documentation "Empty the QUEUE, returning the previous contents."))


;; A simple queue implementation
(defclass simple-queue ()
  ((head :accessor queue-head :initarg :head))
  (:documentation "Simple queue where elements are dequeued from the beginning,
but can be queued either at the end or the beginning."))

(defun simple-queue (&optional contents)
  "Create a simple-queue with given initial CONTENTS."
  (let ((c (cons 0 (copy-list contents))))
    (setf (car c) (last c))
    (make-instance 'simple-queue :head c))))


(with-upgradability ()

;;; Internals of simple-queue
(defgeneric queue-tail (queue)
  (:documentation "the last CONS cell in the QUEUE"))
(defgeneric (setf queue-tail) (new-tail queue)
  (:documentation "set the last CONS cell in the QUEUE"))
(defmethod queue-tail ((q simple-queue))
  (car (queue-head q)))
(defmethod (setf queue-tail) (v (q simple-queue))
  (setf (car (queue-head q)) v))

(defmethod enqueue ((q simple-queue) x)
  (let ((c (list x)))
    (setf (cdr (queue-tail q)) c
          (queue-tail q) c)
    t))
(defmethod enqueue-in-front ((q simple-queue) x)
  (if (empty-p q)
      (enqueue q x)
      (push x (cdr (queue-head q))))
  t)
(defmethod empty-p ((q simple-queue))
  (null (cdr (queue-head q))))
(defmethod dequeue ((q simple-queue))
  (when (null (cdr (queue-head q)))
    (error "Trying to dequeue from an empty queue!"))
  (prog1 (pop (cdr (queue-head q)))
    (when (null (cdr (queue-head q)))
      (setf (queue-tail q) (queue-head q)))))
(defmethod queue-contents ((q simple-queue))
  (copy-list (cdr (queue-head q))))
(defmethod dequeue-all ((q simple-queue))
  (prog1 (cdr (queue-head q))
    (setf (queue-tail q) (queue-head q) (cdr (queue-head q)) nil)))

(defun call-with-queue (fun queue)
  "Repeatedly dequeue a value from QUEUE and call FUN on it until the queue is empty."
  (loop :until (empty-p queue) :do (funcall fun (dequeue queue))))
(defmacro with-queue ((var qvar qval) &body body)
  "Bind QVAR to queue value QVAL, then repeatedly dequeue values from it,
and for each value, bind it to VAR and evaluate BODY."
  `(let ((,qvar ,qval)) (call-with-queue (lambda (,var) ,@body) ,qvar)))


;; Not used in POIU. To be deleted.
(defgeneric enqueue-new (queue value &key test test-not)
  (:documentation "queues the VALUE at the end of the QUEUE if it's not in it already,
as determined by TEST or TEST-NOT."))
(defmethod enqueue-new ((q simple-queue) x &rest keys &key test test-not)
  (declare (ignore test test-not))
  (unless (apply #'find x (cdr (queue-head q)) keys)
    (enqueue q x)))
(defgeneric enqueue-many (queue list))
(defmethod enqueue-many ((q simple-queue) list)
  (dolist (x list) (enqueue q x)) (values))

;; A binary min-heap, ordered by a KEY function returning a real.
;; Used by the scheduler to always pick the earliest ready action in plan order.
(defclass priority-queue ()
  ((heap :initform (make-array 16 :adjustable t :fill-pointer 0) :reader priority-queue-heap)
   (key :initarg :key :reader priority-queue-key))
  (:documentation "Min-heap of items ordered by the real number returned by KEY."))

(defun make-priority-queue (&key (key #'identity))
  (make-instance 'priority-queue :key key))

(defmethod empty-p ((q priority-queue))
  (zerop (fill-pointer (priority-queue-heap q))))
(defmethod size ((q priority-queue))
  (fill-pointer (priority-queue-heap q)))

(defun priority-queue-push (q item)
  (let* ((heap (priority-queue-heap q))
         (key (priority-queue-key q))
         (k (funcall key item))
         (i (vector-push-extend item heap)))
    (loop :while (plusp i)
          :do (let ((parent (floor (1- i) 2)))
                (if (< k (funcall key (aref heap parent)))
                    (setf (aref heap i) (aref heap parent)
                          i parent)
                    (return))))
    (setf (aref heap i) item)
    item))

(defun priority-queue-peek (q)
  (let ((heap (priority-queue-heap q)))
    (and (plusp (fill-pointer heap)) (aref heap 0))))

(defun priority-queue-pop (q)
  (let* ((heap (priority-queue-heap q))
         (key (priority-queue-key q))
         (n (1- (fill-pointer heap))))
    (when (minusp n)
      (error "Trying to pop from an empty priority queue!"))
    (let ((top (aref heap 0))
          (item (aref heap n)))
      (setf (fill-pointer heap) n)
      (when (plusp n)
        (let ((k (funcall key item))
              (i 0))
          (loop
            (let* ((left (1+ (* 2 i)))
                   (right (1+ left))
                   (child
                     (cond
                       ((>= left n) (return))
                       ((and (< right n)
                             (< (funcall key (aref heap right))
                                (funcall key (aref heap left))))
                        right)
                       (t left))))
              (if (< (funcall key (aref heap child)) k)
                  (setf (aref heap i) (aref heap child)
                        i child)
                  (return))))
          (setf (aref heap i) item)))
      top)))

);with-upgradability
