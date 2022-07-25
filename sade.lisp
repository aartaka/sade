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
  (let* ((type `(unsigned-byte ,cell-size))
         (cell-max (1- (expt 2 cell-size)))
         (address-max (1- (expt 2 address-size)))
         (ptr-type `(integer 0 ,address-max))
         (offset-type `(integer ,(- address-max) ,address-max))
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

(defun bf-compile (name stream)
  (compile
   name
   `(lambda ()
      ,(bf stream))))

(defun bf-compile-from-file (name file)
  (with-open-file (stream file)
    (bf-compile name stream)))

(defun bf-compile-from-string (name string)
  (with-input-from-string (stream string)
    (bf-compile name stream)))

(defun bf-eval-file (file)
  (let ((sym (gensym)))
    (bf-compile-from-file sym file)
    (funcall sym)))

(defun bf-eval-string (string)
  (let ((sym (gensym)))
    (bf-compile-from-string sym string)
    (funcall sym)))
