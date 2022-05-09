;;;; bfl.asd

(asdf:defsystem #:bfl
  :description "An infinitely optimizable Brainfuck-to-Lisp compiler."
  :author "Artyom Bologov"
  :license  "BSD 2-Clause"
  :version "0.8.0"
  :serial t
  :components ((:file "package")
               (:file "primitives")
               (:file "match")
               (:file "optimizations")
               (:file "bfl")))

(asdf:defsystem "bfl/cli"
  :description "The CLI tool to compile and run bf files conveniently."
  :depends-on (bfl)
  :build-operation "program-op"
  :build-pathname "bfl"
  :entry-point "bfl::entry-point"
  :components ((:file "cli")))
