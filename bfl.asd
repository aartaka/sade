;;;; bfl.asd

(asdf:defsystem #:bfl
  :description "An infinitely optimizable Brainfuck-to-Lisp compiler."
  :author "Artyom Bologov"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "primitives")
               (:file "match")
               (:file "optimizations")
               (:file "bfl")))
