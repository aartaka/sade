LISP ?= sbcl

.PHONY: all install clean

all: bfl

clean:
	rm bfl

bfl:
	$(LISP) --load bfl.asd --eval '(asdf:load-system :bfl)' --eval '(asdf:load-system :bfl/cli)' --eval '(asdf:make :bfl/cli)' --eval '(quit)'
