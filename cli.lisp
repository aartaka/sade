;;;; cli.lisp

(in-package #:sade)

(defun entry-point ()
  (flet ((info (control &rest args)
           (apply #'format t control args))
         (memp (item list)
           (member item list :test #'equalp)))
    (let* ((args uiop:*command-line-arguments*)
           (argc (length args)))
      (handler-case
          (cond
            ((and (= 1 argc) (memp (first args) '("i" "input")))
             (loop with collected = ""
                   while t
                   for input = (read-line)
                   when (equal input "")
                     do (bf-eval-string collected)
                     and do (setf collected "")
                   else
                     do (setf collected (uiop:strcat collected input))))
            ((and (= 2 argc) (memp (first args) '("o" "optimize")))
             (let ((in (uiop:merge-pathnames* (uiop:parse-native-namestring (second args))
                                              (uiop:getcwd))))
               (info "The optimized code for ~a is~%~a~%"
                     in (with-open-file (i in) (bf i)))

               (info "~%The optimized assembly for ~a is~%" in)
               (let ((name (intern (string-upcase (pathname-name in)))))
                 (bf-compile-from-file name in)
                 (disassemble name))))
            ((and (= 2 argc) (memp (first args) '("r" "run")))
             (load (uiop:parse-native-namestring (second args))))
            ((and (<= 2 argc 3) (memp (first args) '("c" "compile")))
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
            ((or (zerop argc) (memp args '(("h") ("-h") ("help") ("--help"))))
             (info "Sade, an extensible Brainfuck to Lisp compiler.

Usage: sade command [args]

Commands~13tArgs~25tDescription
~2th/help~25tprint this message.
~2to/optimize~13tin~25tshow the optimized code for IN.
~2tc/compile~13tin [out]~25tcompile IN into OUT.
~2tr/run~13tscript~25trun the compiled SCRIPT.
~2ti/input~25tstart an interactive BF shell.

Examples:
# compile hello.bf to hello.XXX (format situation-dependent)
sade c hello.bf hello
# the same thing, yet shorter
sade c hello.bf
# run the previously compiled hello.XXX
sade r hello.XXX
# run the previously compiled hello.XXX inferring it from original file
sade r hello.bf ~%"))
            (t (info "No such command found. Try
sade h

to know the commands there are.~%")))
        (#+sbcl sb-sys:interactive-interrupt
         #+ccl  ccl:interrupt-signal-condition
         #+clisp system::simple-interrupt-condition
         #+ecl ext:interactive-interrupt
         #+allegro excl:interrupt-signal
         ()
          (info "Interrupt received. Quitting...~%")
          (uiop:shell-boolean-exit nil)))))
  (uiop:shell-boolean-exit t))
