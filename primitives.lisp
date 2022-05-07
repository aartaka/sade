;;;; primitives.lisp

(in-package #:bfl)

(declaim (type fixnum %memory-size% %cell-max% %ptr%)
         (type (simple-array fixnum) %memory%))
(defvar %memory-size% 0)
(defvar %memory% (make-array 0 :element-type 'fixnum :initial-element 0 ))
(defvar %cell-max% 0)
(defvar %ptr% 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *primitives* '(lop)))

(defun primitive-p (symbol)
  (member symbol *primitives*))

(defmacro defprimitive (name (&rest args) &body body)
  (push name *primitives*)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
       ,@body)))

(defun getc ()
  (aref %memory% %ptr%))

(defun (setf getc) (value)
  (setf (aref %memory% %ptr%) value))

(defprimitive setc (value)
  (setf (getc) value))

(declaim (ftype (function (fixnum))
                plus minus right left copy copy-from))
(defprimitive plus (amount)
  (setf (getc) (mod (+ (getc) amount) %cell-max%)))

(defprimitive minus (amount)
  (setf (getc) (mod (- (getc) amount) %cell-max%)))

(defprimitive right (amount)
  (setf %ptr% (mod (+ %ptr% amount) %memory-size%)))

(defprimitive left (amount)
  (setf %ptr% (mod (- %ptr% amount) %memory-size%)))

(defprimitive copy (offset)
  (setf (aref %memory% (+ offset %ptr%)) (mod (+ (aref %memory% (+ offset %ptr%)) (getc)) %cell-max%)
        (getc) 0))

(defprimitive copy-from (offset)
  (setf (getc) (mod (+ (aref %memory% (+ offset %ptr%)) (getc)) %cell-max%)
        (aref %memory% (+ offset %ptr%)) 0))

(defprimitive readc ()
  (copy-seq %memory%)
  (setf (getc) (char-code (read-char)))
  (copy-seq %memory%))

(defprimitive printc ()
  (princ (code-char (getc))))

(defmacro lop (&body body)
  `(loop
     (if (zerop (getc))
         (return)
         (progn ,@body))))
