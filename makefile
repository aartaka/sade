LISP ?= sbcl
DESTDIR ?= /usr/bin

.PHONY: all install clean

all: sade

clean:
	rm sade

sade:
	$(LISP) --eval '(require "asdf")' --load sade.asd --eval '(asdf:load-system :sade)' --eval '(asdf:load-system :sade/cli)' --eval '(asdf:make :sade/cli)' --eval '(quit)'

install: sade
	cp sade $(DESTDIR)/
