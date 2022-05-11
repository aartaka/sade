LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size 1024 --no-userinit --non-interactive
endif
LISP_FLAGS ?= $(SBCL_FLAGS)
DESTDIR ?= /usr/bin

.PHONY: all install clean

all: sade

clean:
	rm sade

sade:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load sade.asd --eval '(asdf:load-system :sade)' --eval '(asdf:make :sade)' --eval '(quit)'

install: sade
	cp sade $(DESTDIR)/
