;;;; primitives.lisp

(in-package #:sade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *primitives* (make-hash-table :test #'equal))
  (defvar *commands* (make-hash-table)))

(defstruct primitive
  (char nil :type (or null character))
  (name nil :type (or symbol list))
  (args '() :type list)
  (body '() :type list))

(defmacro defprimitive ((name &optional char) (&rest args) &body body)
  `(let ((primitive (make-primitive :char ,char
                                    :name (quote ,name)
                                    :args (quote ,args)
                                    :body (quote ,body))))
     (setf (gethash (quote ,name) *primitives*) primitive)
     ,(when char
        `(setf (gethash ,char *commands*) primitive))
     primitive))

(defprimitive (getc) ()
  %declarations%
  (the %type% (aref %memory% %ptr%)))

(defprimitive ((setf getc)) (value)
  %declarations%
  (setf (aref %memory% %ptr%) (the %type% value)))

(defprimitive (getco) (offset)
  %declarations%
  (the %type% (aref %memory% (+ %ptr% (the fixnum offset)))))

(defprimitive ((setf getco)) (value offset)
  %declarations%
  (setf (aref %memory% (+ %ptr% (the fixnum offset))) (the %type% value)))

(defprimitive (setc) (value)
  %declarations%
  (setf (getc) (the %type% value)))

(defprimitive (plus #\+) (amount)
  %declarations%
  (setf (getc)
        (the %type% (logand (the %type% (+ (the %type% (getc)) (the %type% amount)))
                            %cell-max%))))

(defprimitive (minus #\-) (amount)
  %declarations%
  (setf (getc)
        (logand (the %type% (- (the %type% (getc)) (the %type% amount)))
                %cell-max%)))

(defprimitive (right #\>) (amount)
  %declarations%
  (setf %ptr% (logand (the fixnum (+ %ptr% (the fixnum amount))) %address-max%)))

(defprimitive (left #\<) (amount)
  %declarations%
  (setf %ptr% (logand (the fixnum (- %ptr% (the fixnum amount))) %address-max%)))

(defprimitive (readc #\,) ()
  (setf (getc) (the %type% (char-code (read-char)))))

(defprimitive (printc #\.) ()
  (princ (code-char (the %type% (getc)))))

(defprimitive (copy) (offset)
  %declarations%
  (setf (getco offset) (logand (the %type% (+ (the %type% (getco offset))
                                              (the %type% (getc))))
                               %cell-max%)
        (getc) 0))

(defprimitive (mult) (offset multiplier)
  %declarations%
  (setf (getco offset) (logand (the %type% (+ (the %type% (getco offset))
                                              (the %type% (* (the %type% (getc)) (the %type% multiplier)))))
                               %cell-max%)
        (getc) 0))

(defprimitive (copy-from) (offset)
  %declarations%
  (setf (getc) (logand (the %type% (+ (the %type% (getco offset))
                                      (the %type% (getc))))
                       %cell-max%)
        (getco offset) 0))

(defprimitive (scan-right) (offset)
  %declarations%
  (loop for index from %ptr% by (the fixnum offset)
        when (zerop (aref %memory% index))
          do (return (setf %ptr% index))))

(defprimitive (scan-left) (offset)
  %declarations%
  (loop for index from %ptr% downto 0 by (the fixnum offset)
        when (zerop (aref %memory% index))
          do (return (setf %ptr% index))))
