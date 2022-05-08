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
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (aref %memory% %ptr%))

(defun (setf getc) (value)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (setf (aref %memory% %ptr%) value))

(defun getco (offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type fixnum offset))
  (aref %memory% (+ %ptr% offset)))

(defun (setf getco) (value offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type fixnum offset value))
  (setf (aref %memory% (+ %ptr% offset)) value))

(defprimitive setc (value)
  (setf (getc) value))

(declaim (ftype (function (fixnum))
                plus minus right left copy copy-from))
(defprimitive plus (amount)
  (setf (getc) (the fixnum (logand (the fixnum (+ (the fixnum (getc)) amount))
                                   %cell-max%))))

(defprimitive minus (amount)
  (setf (getc) (logand (the fixnum (- (the fixnum (getc)) amount)) %cell-max%)))

(defprimitive right (amount)
  (setf %ptr% (logand (the fixnum (+ %ptr% amount)) %address-max%)))

(defprimitive left (amount)
  (setf %ptr% (logand (the fixnum (- %ptr% amount)) %address-max%)))

(defprimitive copy (offset)
  (setf (getco offset) (logand (the fixnum (+ (the fixnum (getco offset))
                                              (the fixnum (getc))))
                               %cell-max%)
        (getc) 0))

(declaim (ftype (function (fixnum fixnum)) mult))
(defprimitive mult (offset multiplier)
  (setf (getco offset) (logand (the fixnum (+ (the fixnum (getco offset))
                                              (the fixnum (* (the fixnum (getc)) multiplier))))
                               %cell-max%)
        (getc) 0))

(defprimitive copy-from (offset)
  (setf (getc) (logand (the fixnum (+ (the fixnum (getco offset))
                                      (the fixnum (getc))))
                       %cell-max%)
        (getco offset) 0))

(defprimitive readc ()
  (setf (getc) (char-code (read-char))))

(defprimitive printc ()
  (princ (code-char (getc))))

(defprimitive scan-right ()
  (setf %ptr% (position 0 %memory% :start %ptr%)))

(defprimitive scan-left ()
  (setf %ptr% (position 0 %memory% :end (1+ %ptr%) :from-end t)))

(defmacro lop (&body body)
  `(loop
     (if (zerop (getc))
         (return)
         (progn ,@body))))
