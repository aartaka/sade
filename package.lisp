;;;; package.lisp

(defpackage #:bfl
  (:use #:cl)
  (:export #:bf #:bf-compile
           #:bf-compile-from-file #:bf-compile-from-string
           #:bf-eval-file #:bf-eval-string))
