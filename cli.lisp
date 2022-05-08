;;;; cli.lisp

(in-package #:bfl)

(defun func ())

(defun entry-point ()
  (flet ((info (control &rest args)
           (apply #'format t control args)))
    (let ((args uiop:*command-line-arguments*))
      (handler-case
          (cond
            ((and (= 1 (length args))
                  (or (equalp (first args) "i")
                      (equalp (first args) "input")))
             (loop with collected = ""
                   while t
                   for input = (read-line)
                   when (equal input "")
                     do (bf-eval-string collected)
                     and do (setf collected "")
                   else
                     do (setf collected (uiop:strcat collected input))))
            ((and (= 2 (length args))
                  (or (equalp (first args) "r")
                      (equalp (first args) "run")))
             (load (uiop:parse-native-namestring (second args))))
            ((and (or (= 2 (length args))
                      (= 3 (length args)))
                  (or (equalp (first args) "c")
                      (equalp (first args) "compile")))
             (let* ((in (uiop:merge-pathnames* (uiop:parse-native-namestring (second args))
                                               (uiop:getcwd)))
                    (out (compile-file-pathname
                          (if (third args)
                              (uiop:merge-pathnames* (third args) (uiop:getcwd))
                              in))))
               (uiop:with-temporary-file
                   (:stream f :pathname p :type "lisp" :keep t)
                 (format f "~s" (with-open-file (i in) (bf i)))
                 :close-stream
                 (compile-file p :output-file out))))
            ((or (null args)
                 (equalp '("h") args)
                 (equalp '("help") args))
             (info "BFL, an extensible Brainfuck to Lisp compiler.

Usage: bfl command [args]

Commands~13tArgs~25tDescription
~2th/help~25tprint this message.
~2tc/compile~13tin [out]~25tcompile IN into OUT.
~2tr/run~13tscript~25trun the compiled SCRIPT.
~2ti/input~25tstart an interactive BF console.

Examples:
# compile hello.bf to hello.XXX (format situation-dependent)
bfl c hello.bf hello
# the same thing, yet shorter
bfl c hello.bf
# run the previously compiled hello.XXX
bfl r hello.XXX
# run the previously compiled hello.XXX inferring it from original file
bfl r hello.bf ~%")))
        (#+sbcl sb-sys:interactive-interrupt
         #+ccl  ccl:interrupt-signal-condition
         #+clisp system::simple-interrupt-condition
         #+ecl ext:interactive-interrupt
         #+allegro excl:interrupt-signal
         ()
          (info "Interrupt received. Quitting...~%")
          (uiop:shell-boolean-exit nil)))))
  (uiop:shell-boolean-exit t))
