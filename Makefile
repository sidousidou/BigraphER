-include Makefile.config

.PHONY: all install

all: _obuild/bigrapher/bigrapher.asm

_obuild/bigrapher/bigrapher.asm: init
	$(OCPBUILD)

bin/bigrapher.ml:
	@echo
	@echo "    ERROR: you need to run ./configure."
	@echo
	@exit 1

init: bin/bigrapher.ml
	$(OCPBUILD) root

clean: 
	$(OCPBUILD) clean
	rm -f *~
	rm -f aclocal.m4
	rm -fr autom4te.cache
	rm -f *.tar.gz

distclean:
	$(MAKE) clean 
	rm -f ocp-build.root*
	rm -f config.status config.log Makefile.config
	rm -f bin/bigrapher.ml bin/bigrapher.ocp

test:
	$(OCPBUILD) test

install: _obuild/bigrapher/bigrapher.asm
	install -m 755 _obuild/bigrapher/bigrapher.asm $(prefix)/bin/bigrapher

uninstall:
	$(OCPBUILD) uninstall
	rm -f $(prefix)/bin/bigrapher

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf

ARCH = bigrapher-$(version)
ARCH_TARGZ = $(ARCH).tar.gz

ARCH_FILES = $(shell git ls-tree --name-only -r HEAD)

prepare-archive:
	rm -f $(ARCH) $(ARCH_TARGZ)
	ln -s . $(ARCH)

complete-archive:
	tar cz $(addprefix $(ARCH)/,$(ARCH_FILES)) > $(ARCH).tar.gz
	rm -f $(ARCH)

$(ARCH_TARGZ):
	@echo "    Preparing files ..."
	$(MAKE) prepare-archive
	$(MAKE) complete-archive

dist: $(ARCH_TARGZ)
	@
