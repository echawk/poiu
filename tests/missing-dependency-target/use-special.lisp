(in-package #:poiu-missing-dependency-target)

(eval-when (:compile-toplevel) (sleep 0.2))

;; Without "specials" loaded, *FACTOR* would be bound lexically here.
(defun use-special () (let ((*factor* 21)) (scaled 2)))
