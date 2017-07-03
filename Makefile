# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

-include Makefile.config

EMACS ?= emacs

emacs-mode: big-mode/big-mode.el
	$(EMACS) --batch --no-init-file -f batch-byte-compile $<

ARCH = bigrapher-$(PKG_VERSION).tar.gz

dist:
	git archive --format=tar --prefix="bigrapher/" HEAD | gzip -n > $(ARCH)
	./scripts/url.sh $(ARCH) > ./opam/url

man: man/bigrapher.1 man/bigrapher-full.1 man/bigrapher-sim.1 man/bigrapher-validate.1
	groff -mandoc -Thtml man/bigrapher.1 > man/bigrapher.html
	groff -mandoc -Thtml man/bigrapher-full.1 > man/bigrapher-full.html
	groff -mandoc -Thtml man/bigrapher-sim.1 > man/bigrapher-sim.html
	groff -mandoc -Thtml man/bigrapher-validate.1 > man/bigrapher-validate.html
	mandoc -mandoc -T html -O style=mandoc.css,man=%N.html -W all  man/test.1 > man/test.html

.PHONY: emacs-mode dist man
