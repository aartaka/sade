;;;; primitives.lisp

(in-package #:bfl)

(declaim (type fixnum %address-size% %address-max% %cell-size% %cell-max% %ptr%)
         (type (simple-array fixnum) %memory%))
(defvar %address-size% 0
  "The size of address space (and thus, memory), in bits.

If it's 8, then memory only has 256 cells.
If it's 16, then memory is 65536.")
(defvar %address-max% 0
  "The maximum address memory can have is usually (1- (expt 2 %address-size%)).")
(defvar %memory% (make-array 0 :element-type 'fixnum :initial-element 0 )
  "The memory array.

Is typed as an array of fixnums, which, on 64-bit OSes should be
enough for any Brainfuck magic, from 1-bit cells and 1-bit memory to
64-bit cells and memory taking up all RAM.")
(defvar %cell-size% 0
  "The size of a cell in bits.

The typical value for it is 8. 8 bits (or a range from 0 to 255) is a
conventional basic Brainfuck cell size. Any other number of bits
fitting the machine word is fine, though.")
(defvar %cell-max% 0
  "The maximum value a cell can get.")
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
