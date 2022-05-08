;;;; bfl.lisp

(in-package #:bfl)

(defun process-commands (stream)
  (optimize-body
   (delete nil
           (loop for char = (read-char stream nil nil)
                 while char
                 collect (case char
                           (#\+ `(plus 1))
                           (#\- `(minus 1))
                           (#\> `(right 1))
                           (#\< `(left 1))
                           (#\, `(readc))
                           (#\. `(printc))
                           (#\] (return commands))
                           (#\[ `(lop ,@(process-commands stream)))
                           (otherwise nil))
                   into commands
                 finally (return commands)))))

(defun bf (stream
           ;; Memory size should be a power of 2 to enable the logand hack in primitives.lisp.
           &key (address-size 15)
             (cell-size 8))
  `(let* ((%address-size% ,address-size)
          (%address-max% ,(1- (expt 2 address-size)))
          (%cell-size% ,cell-size)
          (%cell-max% ,(1- (expt 2 cell-size)))
          (%memory% (make-array ,(expt 2 address-size) :element-type 'fixnum :initial-element 0))
          (%ptr% 0))
     (progn
       ,@(process-commands stream))
     (values %ptr% %memory%)))

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
