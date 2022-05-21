;;;; sade.lisp

(in-package #:sade)

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

(defmacro lop (&body body)
  `(loop
     (if (zerop (getc))
         (return)
         (progn ,@body))))

(defun bf (stream
           &key (address-size 15)
             (cell-size 8)
             (declarations '(declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))))
  (let ((type `(unsigned-byte ,cell-size))
        (cell-max (1- (expt 2 cell-size)))
        (address-max (1- (expt 2 address-size))))
    `(let* ((*memory* (make-array ,(expt 2 address-size) :element-type (quote ,type) :initial-element 0))
            (*ptr* 0))
       (declare (type (integer 0 ,address-max) *ptr*)
                (type (simple-array ,type) *memory*))
       (labels ((getc ()
                  ,declarations
                  (the ,type (aref *memory* *ptr*)))
                ((setf getc) (value)
                  ,declarations
                  (declare (type ,type value))
                  (setf (aref *memory* *ptr*) value))
                (getco (offset)
                  ,declarations
                  (declare (type fixnum offset))
                  (the ,type (aref *memory* (+ *ptr* offset))))
                ((setf getco) (value offset)
                  ,declarations
                  (declare (type fixnum offset)
                           (type ,type value))
                  (setf (aref *memory* (+ *ptr* offset)) value))
                (setc (value)
                  ,declarations
                  (declare (type ,type value))
                  (setf (getc) value))
                (plus (amount)
                  ,declarations
                  (declare (type ,type amount))
                  (setf (getc)
                        (the ,type (logand (the ,type (+ (the ,type (getc)) (the ,type amount)))
                                           ,cell-max))))
                (minus (amount)
                  ,declarations
                  (declare (type ,type amount))
                  (setf (getc)
                        (logand (the ,type (- (the ,type (getc)) (the ,type amount)))
                                ,cell-max)))
                (right (amount)
                  ,declarations
                  (declare (type fixnum amount))
                  (setf *ptr* (logand (the ,type (+ *ptr* (the ,type amount))) ,address-max)))
                (left (amount)
                  ,declarations
                  (declare (type fixnum amount))
                  (setf *ptr* (logand (the ,type (- *ptr* (the ,type amount))) ,address-max)))
                (readc ()
                  (setf (getc) (the ,type (char-code (read-char)))))
                (printc ()
                  (princ (code-char (the ,type (getc)))))
                (copy (offset)
                  ,declarations
                  (declare (type fixnum offset))
                  (setf (getco offset) (logand (the ,type (+ (the ,type (getco offset))
                                                             (the ,type (getc))))
                                               ,cell-max)
                        (getc) 0))
                (mult (offset multiplier)
                  ,declarations
                  (declare (type fixnum offset)
                           (type ,type multiplier))
                  (setf (getco offset) (logand (the ,type (+ (the ,type (getco offset))
                                                             (the ,type (* (the ,type (getc)) multiplier))))
                                               ,cell-max)
                        (getc) 0))
                (copy-from (offset)
                  ,declarations
                  (declare (type fixnum offset))
                  (setf (getc) (logand (the ,type (+ (the ,type (getco offset))
                                                     (the ,type (getc))))
                                       ,cell-max)
                        (getco offset) 0))
                (scan-right (offset)
                  ,declarations
                  (declare (type fixnum offset))
                  (loop for index from *ptr* by offset
                        when (zerop (aref *memory* index))
                          do (return (setf *ptr* index))))
                (scan-left (offset)
                  ,declarations
                  (declare (type fixnum offset))
                  (loop for index from *ptr* downto 0 by offset
                        when (zerop (aref *memory* index))
                          do (return (setf *ptr* index)))))
         (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
                  (inline getc (setf getc) getco (setf getco) setc
                          plus minus left right
                          copy copy-from mult scan-left scan-right))
         (progn
           ,@(process-commands stream))
         (values *ptr* *memory*)))))

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
