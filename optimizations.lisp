;;;; optimizations.lisp

(in-package #:bfl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimizations* '())
  (defvar *bindings* (make-hash-table)))

(defmacro defoptimization (name match &body body)
  (pushnew (cons match name) *optimizations* :key #'car :test #'equalp)
  `(progn
     (pushnew (cons (quote ,match) (quote ,name)) *optimizations* :key #'car :test #'equalp)
     (defun ,name ()
       (maphash (lambda (var val) (setf (symbol-value var) val)) *bindings*)
       ,@body)))

(defun optimize-body (body)
  (loop with exprs = body
        while exprs
        for (name position bindings)
          = (loop for (spec . name) in *optimizations*
                  when (match spec exprs)
                    do (return (cons name (multiple-value-list (match spec exprs))))
                  finally (return nil))
        when name
          do (let* ((*bindings* bindings)
                    (new-commands (funcall name))
                    (new-exprs (append new-commands (subseq exprs (1+ position)))))
               (setf (car exprs) (car new-exprs)
                     (cdr exprs) (cdr new-exprs)))
        else
          do (setf exprs (cdr exprs))
        finally (return body)))

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
    ((lop (right x) (plus y) (left x) (minus 1)) **)
  (if (= y 1)
      `((copy ,x))
      `((mult ,x ,y))))

(defoptimization copy-left
    ((lop (left x) (plus y) (right x) (minus 1)) **)
  (if (= y 1)
      `((copy ,(- x)))
      `((mult ,(- x) ,y))))

(defoptimization copy-right-inverted
    ((lop (minus 1) (right x) (plus y) (left x)) **)
  (if (= y 1)
      `((copy ,x))
      `((mult ,x ,y))))

(defoptimization copy-left-inverted
    ((lop (minus 1) (left x) (plus y) (right x)) **)
  (if (= y 1)
      `((copy ,(- x)))
      `((mult ,(- x) ,y))))

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
