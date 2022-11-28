;;;; primitives.lisp

(in-package #:sade)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *primitives* (make-hash-table :test #'equal))
  (defvar *commands* (make-hash-table)))

(defstruct primitive
  "The type to store the information on Sade BF primitives in.
The slots are:
- CHAR -- the BF character corresponding to the primitive.
- NAME -- the Lisp symbol denoting this primitive.
- ARGS -- the arglist of this primitive.
- DEFAULT-ARGS -- the default args to be passed to the parsed primitive.
- BODY -- the actual code to run in the primitive. Can contain magic symbols:
  - %TYPE% -- the current type of the BF values.
    Is replaced by the actual type when compiling Lisp code.
  - %PTR-TYPE% -- the type of the memory pointer.
    Is replaced by the actual type when compiling Lisp code.
  - %OFFSET-TYPE% -- the type for offsets from the pointer.
    Is replaced by the actual type when compiling Lisp code.
  - %DECLARATIONS% -- current optimizations settings and other declarations.
    Is replaced by the actual declarations when compiling Lisp code.
  - %MEMORY% -- the memory tape.
  - %PTR% -- the current position on the memory tape."
  (char nil :type (or null character))
  (name nil :type (or symbol list))
  (args '() :type list)
  (default-args '() :type list)
  (body '() :type list))

(defmacro defprimitive (name-and-args (&rest args) &body body)
  (destructuring-bind (name &optional char default-args)
      (uiop:ensure-list name-and-args)
    `(let ((primitive (make-primitive :char ,char
                                      :name (quote ,name)
                                      :args (quote ,args)
                                      :default-args (quote ,default-args)
                                      :body (quote ,body))))
       (setf (gethash (quote ,name) *primitives*) primitive)
       ,(when char
          `(setf (gethash ,char *commands*) primitive))
       primitive)))

(defprimitive (getc) ()
  %declarations%
  (the %type% (aref %memory% %ptr%)))

(defprimitive ((setf getc)) (value)
  %declarations%
  (setf (aref %memory% %ptr%) (the %type% value)))

(defprimitive (getco) (offset)
  %declarations%
  (the %type% (aref %memory% (logand (+ %ptr% (the %offset-type% offset)) %address-max%))))

(defprimitive ((setf getco)) (value offset)
  %declarations%
  (setf (aref %memory% (logand (+ %ptr% (the %offset-type% offset)) %address-max%))
        (the %type% value)))

(defprimitive (setc) (value)
  %declarations%
  (setf (getc) (the %type% value)))

(defprimitive (plus #\+ (1)) (amount)
  %declarations%
  (setf (getc)
        (the %type% (logand (+ (the %type% (getc)) (the %type% amount))
                            %cell-max%))))

(defprimitive (minus #\- (1)) (amount)
  %declarations%
  (setf (getc)
        (logand (- (the %type% (getc)) (the %type% amount))
                %cell-max%)))

(defprimitive (right #\> (1)) (amount)
  %declarations%
  (setf %ptr% (logand (+ %ptr% (the %ptr-type% amount)) %address-max%)))

(defprimitive (left #\< (1)) (amount)
  %declarations%
  (setf %ptr% (logand (- %ptr% (the %ptr-type% amount)) %address-max%)))

(defprimitive (readc #\,) ()
  (setf (getc) (the %type% (min (char-code (read-char *standard-input* nil #\Null))
                                %cell-max%))))

(defprimitive (printc #\.) ()
  (princ (code-char (the %type% (getc))))
  (force-output))

(defprimitive (copy) (offset)
  %declarations%
  (setf (getco offset) (logand (+ (the %type% (getco offset))
                                  (the %type% (getc)))
                               %cell-max%)
        (getc) 0))

(defprimitive (mult) (offset multiplier)
  %declarations%
  (setf (getco offset) (logand (+ (the %type% (getco offset))
                                  (* (the %type% (getc)) (the %type% multiplier)))
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
  (loop for index from (the %ptr-type% %ptr%) upto %address-max%
            by (the %offset-type% offset)
        when (zerop (aref %memory% index))
          do (return (setf %ptr% index))
             ;; FIXME: SBCL deletes this somewhy.
        finally (loop for index to (the %ptr-type% (1- %ptr%))
                        by (the %offset-type% offset)
                      when (zerop (aref %memory% index))
                        do (return (setf %ptr% index)))))

(defprimitive (scan-left) (offset)
  %declarations%
  (loop for index from (the %ptr-type% %ptr%) downto 0
          by (the %offset-type% offset)
        when (zerop (aref %memory% index))
          do (return (setf %ptr% index))
        finally (loop for index from (the %ptr-type% %address-max%)
                        downto (the %ptr-type% (1+ %ptr%))
                          by (the %offset-type% offset)
                      when (zerop (aref %memory% index))
                        do (return (setf %ptr% index)))))
