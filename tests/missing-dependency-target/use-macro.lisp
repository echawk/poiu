(in-package #:poiu-missing-dependency-target)

;; Give the worker compiling "macros" time to be slower, if anything.
(eval-when (:compile-toplevel) (sleep 0.2))

(defun use-twice () (twice 21))
