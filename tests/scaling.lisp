;;; Scaling benchmark for POIU.
;;;
;;; Generates synthetic ASDF systems with various dependency graph shapes, then builds each
;;; one from scratch in a fresh Lisp process, sequentially with plain ASDF and in parallel
;;; with POIU at various worker counts, and reports wall-clock times and speedups.
;;; Each build also checks that the loaded system computes the expected result.
;;;
;;; Usage: sh tests/run-scaling-benchmark.sh [shape ...]
;;; Shapes: wide layered chain tiny large (default: all).
;;; Environment: POIU_SCALING_WORKERS (default "1 2 4 8 ncpus"),
;;;              POIU_SCALING_REPEAT (default 1; the best time is reported).
(in-package #:cl-user)

(require :asdf)

(defparameter *tests-root*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename* *default-pathname-defaults*)))
(defparameter *repo-root* (uiop:merge-pathnames* #p"../" *tests-root*))
(defparameter *work-root*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "POIU_SCALING_ROOT")
       (uiop:merge-pathnames* #p".cache/scaling/" *repo-root*))))

;;; Generating systems

(defun file-name (i) (format nil "f~4,'0D" i))

(defun write-source-file (directory name weight)
  "Write a file defining function NAME whose compilation takes time proportional to WEIGHT,
and that returns WEIGHT when called."
  (with-open-file (s (uiop:subpathname directory (format nil "~A.lisp" name))
                     :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-case* :downcase) (*package* (find-package :cl-user)))
        (format s "(in-package #:poiu-scaling)~%~%")
        ;; WEIGHT*10 helper functions, each of which takes a few milliseconds to compile.
        (dotimes (j (* 10 weight))
          (format s "~S~%"
                  `(defun ,(intern (format nil "~:@(~A-~D~)" name j)) (x)
                     (let ((acc 0))
                       ,@(loop :for k :below 40
                               :collect `(setf acc (+ acc (* x ,k) (floor x ,(1+ k)))))
                       acc))))
        (format s "~S~%" `(defun ,(intern (string-upcase name)) () ,weight))))))

(defun write-system (name files &key (weight 4))
  "FILES is a list of (file-name . dependency-names). Writes system NAME, whose RUN function
returns the sum of the weights of all files."
  (let ((directory (uiop:subpathname *work-root* (format nil "~A/" name))))
    (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist directory)
    (with-open-file (s (uiop:subpathname directory "package.lisp")
                       :direction :output :if-exists :supersede)
      (format s "(defpackage #:poiu-scaling (:use #:cl) (:export #:run))~%"))
    (loop :for (file) :in files :do (write-source-file directory file weight))
    (with-open-file (s (uiop:subpathname directory "final.lisp")
                       :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase))
        (format s "(in-package #:poiu-scaling)~%~%(defun run () (+ ~{(~A)~^ ~}))~%"
                (mapcar #'first files))))
    (with-open-file (s (uiop:subpathname directory (format nil "~A.asd" name))
                       :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase))
        (format s "~S~%"
                `(asdf:defsystem ,name
                   :components
                   ((:file "package")
                    ,@(loop :for (file . deps) :in files
                            :collect `(:file ,file :depends-on ("package" ,@deps)))
                    (:file "final" :depends-on ,(mapcar #'first files)))))))
    (values directory (* weight (length files)))))

(defun shape-files (shape)
  "Return the list of (file . dependencies) and the weight for a shape name."
  (flet ((names (n) (loop :for i :below n :collect (file-name i))))
    (cond
      ;; N independent files of moderate size: the ideal case for parallelism.
      ((string= shape "wide")
       (values (mapcar #'list (names 60)) 10))
      ;; Layers of files, each depending on two files of the previous layer.
      ((string= shape "layered")
       (let ((width 12) (depth 5))
         (values (loop :for layer :below depth
                       :append (loop :for i :below width
                                     :for n = (+ (* layer width) i)
                                     :collect (cons (file-name n)
                                                    (when (plusp layer)
                                                      (list (file-name (+ (* (1- layer) width) i))
                                                            (file-name (+ (* (1- layer) width)
                                                                          (mod (+ i 5) width))))))))
                 10)))
      ;; A serial chain: no parallelism is possible, POIU must not slow it down.
      ((string= shape "chain")
       (values (loop :for i :below 30
                     :collect (cons (file-name i) (when (plusp i) (list (file-name (1- i))))))
               6))
      ;; Many tiny files: measures per-action scheduling overhead.
      ((string= shape "tiny")
       (values (mapcar #'list (names 1000)) 0))
      ;; Many moderate files: throughput at scale.
      ((string= shape "large")
       (values (mapcar #'list (names 400)) 3))
      (t (error "Unknown shape ~S" shape)))))

;;; Running builds

(defun run-build (system directory mode workers)
  "Build SYSTEM from scratch in a fresh Lisp process. Return elapsed seconds and statistics."
  (let* ((cache (uiop:subpathname *work-root* (format nil "cache/~A-~A-~A/" system mode workers)))
         (form
           `(progn
              (require :asdf)
              (push ,(namestring *repo-root*) asdf:*central-registry*)
              (push ,(namestring directory) asdf:*central-registry*)
              ,@(when (eq mode :parallel)
                  `((asdf:load-system :poiu)
                    (setf (symbol-value (find-symbol "*MAX-FORKS*" "POIU/FORK")) ,workers)))
              (let ((start (get-internal-real-time)))
                (asdf:load-system ,system :force t)
                (let ((elapsed (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second))
                      (result (funcall (find-symbol "RUN" "POIU-SCALING"))))
                  (let ((*print-pretty* nil))
                    (format t "~&RESULT ~S ~,3F ~S~%" result (float elapsed)
                            (and (find-package "POIU")
                                 (symbol-value
                                  (find-symbol "*LAST-BUILD-STATISTICS*" "POIU"))))))))))
    (uiop:delete-directory-tree cache :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist cache)
    ;; Warm up: compile POIU itself into this cache first, outside the measurement.
    (when (eq mode :parallel)
      (uiop:run-program
       (list "env" (format nil "XDG_CACHE_HOME=~A" (namestring cache))
             "sbcl" "--noinform" "--non-interactive" "--no-userinit" "--no-sysinit"
             "--eval" "(require :asdf)"
             "--eval" (format nil "(push ~S asdf:*central-registry*)" (namestring *repo-root*))
             "--eval" "(asdf:load-system :poiu)")
       :output nil :error-output nil))
    (let ((output
            (uiop:run-program
             (list "env" (format nil "XDG_CACHE_HOME=~A" (namestring cache))
                   "sbcl" "--noinform" "--non-interactive" "--no-userinit" "--no-sysinit"
                   ;; ASDF must exist before the form that uses it is read.
                   "--eval" "(require :asdf)"
                   "--eval" (with-standard-io-syntax (prin1-to-string form)))
             :output :string :error-output :output :ignore-error-status t)))
      (let ((line (find-if #'(lambda (l) (uiop:string-prefix-p "RESULT " l))
                           (uiop:split-string output :separator '(#\Newline)))))
        (unless line
          (error "Build of ~A (~A, ~A workers) failed:~%~A" system mode workers output))
        (with-input-from-string (s line :start 7)
          (values (read s) (read s) (read s)))))))

(defun best-build (system directory mode workers repeat expected)
  (loop :with best = nil :with best-stats = nil
        :repeat repeat
        :do (multiple-value-bind (result elapsed stats) (run-build system directory mode workers)
              (unless (eql result expected)
                (error "~A (~A, ~A workers) computed ~S instead of ~S"
                       system mode workers result expected))
              (when (or (null best) (< elapsed best))
                (setf best elapsed best-stats stats)))
        :finally (return (values best best-stats))))

(defun default-worker-counts ()
  (let ((ncpus (or (ignore-errors (uiop:symbol-call :poiu/fork :ncpus)) 8)))
    (remove-duplicates (remove-if (lambda (n) (> n ncpus)) (list 1 2 4 8 ncpus))
                       :from-end t)))

(defun main (shapes)
  (pushnew *repo-root* asdf:*central-registry* :test #'equal)
  (asdf:load-system :poiu) ; for ncpus
  (setf asdf/plan:*plan-class* 'asdf/plan:sequential-plan)
  (let ((workers (let ((env (uiop:getenv "POIU_SCALING_WORKERS")))
                   (if env
                       (mapcar #'parse-integer (uiop:split-string env :separator " "))
                       (default-worker-counts))))
        (repeat (or (ignore-errors (parse-integer (uiop:getenv "POIU_SCALING_REPEAT"))) 1))
        (failures 0))
    (format t "~&cpus=~A workers=~A repeat=~A~%" (uiop:symbol-call :poiu/fork :ncpus) workers repeat)
    (dolist (shape shapes)
      (multiple-value-bind (files weight) (shape-files shape)
        (let ((system (format nil "poiu-scaling-~A" shape)))
          (multiple-value-bind (directory expected) (write-system system files :weight weight)
            (let ((sequential (best-build system directory :sequential 0 repeat expected)))
              (format t "~&~%shape=~A files=~D~%" shape (length files))
              (format t "  ~12A ~8,2Fs~%" "sequential" sequential)
              (dolist (n workers)
                (multiple-value-bind (elapsed stats)
                    (best-build system directory :parallel n repeat expected)
                  (format t "  ~12A ~8,2Fs  speedup ~5,2Fx  forks=~D bg=~D fg=~D retried=~D~%"
                          (format nil "poiu x~D" n) elapsed (/ sequential elapsed)
                          (getf stats :forks) (getf stats :background)
                          (getf stats :foreground) (getf stats :retried))
                  (when (plusp (getf stats :retried 0))
                    (incf failures)))))))))
    (finish-output)
    (unless (zerop failures)
      (error "~D builds had background failures" failures))))

(main (or (remove "" (uiop:split-string (or (uiop:getenv "POIU_SCALING_SHAPES") "")
                                          :separator " ")
                  :test #'string=)
          (list "wide" "layered" "chain" "tiny" "large")))
