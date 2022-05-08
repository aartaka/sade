;;;; optimizations.lisp

(in-package #:bfl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimizations* '())
  (defvar *bindings* nil))

(defmacro defoptimization (name match &body body)
  (pushnew (cons match name) *optimizations* :key #'car :test #'equalp)
  `(defmacro ,name ()
     (loop for var being the hash-keys of *bindings*
             using (hash-value val)
           do (setf (symbol-value var) val))
     ,@body))

(defun optimize-body (body)
  (loop with new-body = (copy-tree body)
        with exprs = new-body
        while exprs
        for (name position bindings)
          = (loop for (spec . macro-name) in *optimizations*
                  when (match spec exprs)
                    do (return (cons macro-name (multiple-value-list (match spec exprs)))))
        when name
          do (let* ((*bindings* bindings)
                    (new-commands (macroexpand-1 `(,name)))
                    (new-exprs (append new-commands (subseq exprs (1+ position)))))
               (setf (car exprs) (car new-exprs)
                     (cdr exprs) (cdr new-exprs)))
        else
          do (setf exprs (cdr exprs))
        finally (return new-body)))

(defvar x nil)
(defvar y nil)

(defoptimization pluses
    ((plus x) (plus y) **)
  `((plus ,(+ x y))))

(defoptimization minuses
    ((minus x) (minus y) **)
  `((minus ,(+ x y))))

(defoptimization rights
    ((right x) (right y) **)
  `((right ,(+ x y))))

(defoptimization lefts
    ((left x) (left y) **)
  `((left ,(+ x y))))

(defoptimization empty
    ((lop (minus *)) **)
  `((setc 0)))

(defoptimization init
    ((setc y) (plus x) **)
  `((setc ,(+ y x))))

(defoptimization copy-right
    ((lop (right x) (plus 1) (left x) (minus 1)) **)
  `((copy ,x)))

(defoptimization copy-left
    ((lop (left x) (plus 1) (right x) (minus 1)) **)
  `((copy ,(- x))))

(defoptimization copy-right-inverted
    ((lop (minus 1) (right x) (plus 1) (left x)) **)
  `((copy ,x)))

(defoptimization copy-left-inverted
    ((lop (minus 1) (left x) (plus 1) (right x)) **)
  `((copy ,(- x))))

(defoptimization copy-from-right
    ((left x) (copy x) (right x) **)
  `((copy-from ,x)))

(defoptimization copy-from-left
    ((right x) (copy y) (left x) **)
  (if (eq x (- y))
      `((copy-from ,(- x)))
      `((right ,x) (copy ,y) (left ,x))))

(defoptimization scan-left-1
    ((lop (left 1)) **)
  `((scan-left)))

(defoptimization scan-right-1
    ((lop (right 1)) **)
  `((scan-right)))
