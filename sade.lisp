;;;; sade.lisp

(in-package #:sade)

(defun process-commands (stream)
  (optimize-body
   (loop for char = (read-char stream nil nil)
         for primitive = (gethash char *commands*)
         while char
         when primitive
           collect (cons (primitive-name primitive)
                         (primitive-default-args primitive))
             into commands
         else when (eql char #\])
                do (return commands)
         else when (eql char #\[)
                collect `(lop ,@(process-commands stream))
                  into commands
         finally (return commands))))

(defmacro lop (&body body)
  `(loop
     (if (zerop (getc))
         (return)
         (progn ,@body))))

(defun bf (stream
           &key (address-size 15)
             (cell-size 8)
             (declarations '(declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))))
  "Read the Brainfuck code from STREAM and compile it with the given parameters:

ADDRESS-SIZE -- the number of bits for the address storage. Default to 15.
CELL-SIZE -- the number of bits that fit inside one memory cell. Defaults to 8.
DECLARATIONS -- Lisp compiler declarations that allow fine-tuning the
  relations between speed, safety, space etc."
  (let* ((type `(unsigned-byte ,cell-size))
         (cell-max (1- (expt 2 cell-size)))
         (address-max (1- (expt 2 address-size)))
         (ptr-type `(unsigned-byte ,address-size))
         (offset-type `(signed-byte ,(1+ address-size)))
         (body (process-commands stream)))
    `(let* ((%memory% (make-array ,(expt 2 address-size) :element-type (quote ,type) :initial-element 0))
            (%ptr% 0))
       ,declarations
       (declare (type ,ptr-type %ptr%)
                (type (simple-array ,type) %memory%))
       (labels (,@(loop for name being the hash-key of *primitives*
                          using (hash-value primitive)
                        collect `(,(primitive-name primitive) (,@(primitive-args primitive))
                                  ,@(subst declarations '%declarations%
                                           (subst address-max '%address-max%
                                                  (subst cell-max '%cell-max%
                                                         (subst type '%type%
                                                                (subst offset-type '%offset-type%
                                                                       (subst ptr-type '%ptr-type%
                                                                              (primitive-body primitive))))))))))
         (declare (inline ,@(loop for name being the hash-key of *primitives* collect name)))
         (progn ,@body)
         (values %ptr% %memory%)))))

(defun bf-compile (name stream &rest args &key &allow-other-keys)
  "Compile Brainfuck code from STREAM as a function NAME."
  (compile
   name
   `(lambda ()
      ,(apply #'bf stream args))))

(defun bf-compile-from-file (name file &rest args &key &allow-other-keys)
  "Compile Brainfuck code from FILE as a function NAME."
  (with-open-file (stream file)
    (apply #'bf-compile name stream args)))

(defun bf-compile-from-string (name string &rest args &key &allow-other-keys)
  "Compile Brainfuck code from STRING as a function NAME."
  (with-input-from-string (stream string)
    (apply #'bf-compile name stream args)))

(defun bf-eval-file (file &rest args &key &allow-other-keys)
  "Run Brainfuck code from FILE."
  (let ((sym (gensym)))
    (apply #'bf-compile-from-file sym file args)
    (funcall sym)))

(defun bf-eval-string (string &rest args &key &allow-other-keys)
  "Run Brainfuck code from STRING."
  (let ((sym (gensym)))
    (apply #'bf-compile-from-string sym string args)
    (funcall sym)))
