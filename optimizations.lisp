;;;; optimizations.lisp

(in-package #:sade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimizations* '())
  (defvar *bindings* (make-hash-table)))

(defmacro defoptimization (name match &body body)
  (pushnew name *optimizations*)
  (let ((arg-name (gensym)))
    `(progn
       (pushnew (quote ,name) *optimizations*)
       (defun ,name (,arg-name)
         (ignore-errors
          (destructuring-bind
            ,match ,arg-name
            (values (progn ,@body) match?)))))))

(defun optimize-body (body)
  (loop for exprs = body then (rest exprs)
        while exprs
        do (loop with optimizations = (reverse *optimizations*)
                 for name = (pop optimizations)
                 while name
                 for (new-body match?) = (multiple-value-list (funcall name exprs))
                 when (and match? (not (typep match? 'error)))
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
     &aux (match? (and (eq 'lop lop)
                       (eq 'minus minus))))
  `((setc 0) ,@forms))

(defoptimization init
    ((setc y) (plus x)
     &rest forms
     &aux (match? (and (eq setc 'setc)
                       (eq plus 'plus))))
  `((setc ,(+ y x)) ,@forms))

(defoptimization copy-right
    ((lop (right x) (plus y) (left x) (minus one))
     &rest forms
     &aux (match? (and (= one 1)
                       (eq lop 'lop)
                       (eq right 'right)
                       (eq plus 'plus)
                       (eq left 'left)
                       (eq minus 'minus))))
  (if (= y 1)
      `((copy ,x) ,@forms)
      `((mult ,x ,y) ,@forms)))

(defoptimization copy-left
    ((lop (left x) (plus y) (right x) (minus one))
     &rest forms
     &aux (match? (and (= one 1)
                       (eq lop 'lop)
                       (eq right 'right)
                       (eq plus 'plus)
                       (eq left 'left)
                       (eq minus 'minus))))
  (if (= y 1)
      `((copy ,(- x)) ,@forms)
      `((mult ,(- x) ,y) ,@forms)))

(defoptimization copy-right-inverted
    ((lop (minus one) (right x) (plus y) (left x))
     &rest forms
     &aux (match? (and (= one 1)
                       (eq lop 'lop)
                       (eq right 'right)
                       (eq plus 'plus)
                       (eq left 'left)
                       (eq minus 'minus))))
  (if (= y 1)
      `((copy ,x) ,@forms)
      `((mult ,x ,y) ,@forms)))

(defoptimization copy-left-inverted
    ((lop (minus one) (left x) (plus y) (right x))
     &rest forms
     &aux (match? (and (= one 1)
                       (eq lop 'lop)
                       (eq right 'right)
                       (eq plus 'plus)
                       (eq left 'left)
                       (eq minus 'minus))))
  (if (= y 1)
      `((copy ,(- x)) ,@forms)
      `((mult ,(- x) ,y) ,@forms)))

(defoptimization copy-from-right
    ((left x) (copy x) (right x)
     &rest forms
     &aux (match? (and (eq right 'right)
                       (eq copy 'copy)
                       (eq left 'left))))
  `((copy-from ,x) ,@forms))

(defoptimization copy-from-left
    ((right x) (copy y) (left x)
     &rest forms
     &aux (match? (and (eq x (- y))
                       (eq right 'right)
                       (eq copy 'copy)
                       (eq left 'left))))
  `((copy-from ,(- x)) ,@forms))

(defoptimization scan-left-loop
    ((lop (left x))
     &rest forms
     &aux (match? (and (eq lop 'lop)
                       (eq left 'left))))
  `((scan-left ,x) ,@forms))

(defoptimization scan-right-loop
    ((lop (right x))
     &rest forms
     &aux (match? (and (eq lop 'lop)
                       (eq right 'right))))
  `((scan-right ,x) ,@forms))

;; TODO:
;; - Substraction optimizations, for e.g. [<->-]
;; - Shift loops, like [[<+>-]>]
;; - Setting loops, like [[-]++>]
;; - Reduction loops, like [->]
;; - Complex copying/multiplication, like [>++>+<<-]
;; - Optimizations of patterns from https://esolangs.org/wiki/Brainfuck_algorithms
