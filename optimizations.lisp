;;;; optimizations.lisp

(in-package #:sade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimizations* '()))

(defmacro defoptimization (name match &body body)
  (pushnew name *optimizations*)
  (let ((arg-name (gensym)))
    `(progn
       (pushnew (quote ,name) *optimizations*)
       (defun ,name (,arg-name)
         (handler-case
             (destructuring-bind
               ,match ,arg-name
               (values (progn ,@body) match?))
           (t () (values nil nil)))))))

(defun optimize-body (body)
  (loop for exprs = body then (rest exprs)
        while exprs
        do (loop with optimizations = (reverse *optimizations*)
                 for name = (pop optimizations)
                 while name
                 for (new-body match?) = (multiple-value-list (funcall name exprs))
                 when match?
                   do (setf exprs new-body
                            optimizations (append optimizations (list name))))
        collect (first exprs)))

(defoptimization duplicates
    ((prim1 x) (prim2 y)
     &rest forms
     &aux (match?
           (and (eq prim1 prim2)
                (member prim1 '(plus minus left right)))))
  `((,prim1 ,(+ x y)) ,@forms))

(defoptimization empty
    ((lop (minus x))
     &rest forms
     &aux (match? (and (equalp (list lop minus)
                               '(lop minus))
                       (numberp x))))
  `((setc 0) ,@forms))

(defoptimization init
    ((setc y) (plus x)
     &rest forms
     &aux (match? (equalp (list setc plus)
                          '(setc plus))))
  `((setc ,(+ y x)) ,@forms))

(defoptimization copy-right
    ((lop (right x1) (plus y) (left x2) (minus one))
     &rest forms
     &aux (match? (and (equalp (list lop right plus left minus one)
                               '(lop right plus left minus 1))
                       (= x1 x2))))
  (if (= y 1)
      `((copy ,x1) ,@forms)
      `((mult ,x1 ,y) ,@forms)))

(defoptimization copy-left
    ((lop (left x1) (plus y) (right x2) (minus one))
     &rest forms
     &aux (match? (and (equalp (list lop left plus right minus one)
                               '(lop left plus right minus 1))
                       (= x1 x2))))
  (if (= y 1)
      `((copy ,(- x1)) ,@forms)
      `((mult ,(- x1) ,y) ,@forms)))

(defoptimization copy-right-inverted
    ((lop (minus one) (right x1) (plus y) (left x2))
     &rest forms
     &aux (match? (and (equalp (list one lop minus right plus left)
                               '(1 lop minus right plus left))
                       (= x1 x2))))
  (if (= y 1)
      `((copy ,x1) ,@forms)
      `((mult ,x1 ,y) ,@forms)))

(defoptimization copy-left-inverted
    ((lop (minus one) (left x1) (plus y) (right x2))
     &rest forms
     &aux (match? (and (equalp (list lop minus one left plus right)
                               '(lop minus 1 left plus right))
                       (= x1 x2))))
  (if (= y 1)
      `((copy ,(- x1)) ,@forms)
      `((mult ,(- x1) ,y) ,@forms)))

(defoptimization copy-from-right
    ((left x1) (copy x2) (right x3)
     &rest forms
     &aux (match? (and (equalp (list right copy left)
                               '(right copy left))
                       (= x1 x2 x3))))
  `((copy-from ,x1) ,@forms))

(defoptimization copy-from-left
    ((right x1) (copy y) (left x2)
     &rest forms
     &aux (match? (and (equalp (list right copy left)
                               '(right copy left))
                       (= x1 x2)
                       (eq x1 (- y)))))
  `((copy-from ,(- x1)) ,@forms))

(defoptimization scan-left-loop
    ((lop (left x))
     &rest forms
     &aux (match? (equalp (list lop left)
                          '(lop left))))
  `((scan-left ,x) ,@forms))

(defoptimization scan-right-loop
    ((lop (right x))
     &rest forms
     &aux (match? (equalp (list lop right)
                          '(lop right))))
  `((scan-right ,x) ,@forms))

;; TODO:
;; - Substraction optimizations, for e.g. [<->-]
;; - Shift loops, like [[<+>-]>]
;; - Setting loops, like [[-]++>]
;; - Reduction loops, like [->]
;; - Complex copying/multiplication, like [>++>+<<-]
;; - Optimizations of patterns from https://esolangs.org/wiki/Brainfuck_algorithms
