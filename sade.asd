;;;; sade.asd

(asdf:defsystem #:sade
  :description "An infinitely optimizable Brainfuck-to-Lisp compiler."
  :author "Artyom Bologov"
  :license  "BSD 2-Clause"
  :version "0.8.0"
  :serial t
  :components ((:file "package")
               (:file "primitives")
               (:file "match")
               (:file "optimizations")
               (:file "sade")))

(asdf:defsystem "sade/cli"
  :description "The CLI tool to compile and run bf files conveniently."
  :depends-on (sade)
  :build-operation "program-op"
  :build-pathname "sade"
  :entry-point "sade::entry-point"
  :components ((:file "cli")))
