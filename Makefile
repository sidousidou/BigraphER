-include Makefile.config

.PHONY: all install

all: #call_conf
ifeq (,$(wildcard ocp-build.root))
	$(OCPBUILD) root
endif
	$(OCPBUILD) -njobs 5

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
	rm -f bin/cmd.ml bin/bigrapher.ocp man/bigrapher.1 lib/bigraph.ocp

test:
	$(OCPBUILD) test

install: all _obuild/bigrapher/bigrapher.asm
	$(OCPBUILD) install bigraph	
	install -m 755 _obuild/bigrapher/bigrapher.asm $(prefix)/bin/bigrapher
	install -m 755 man/bigrapher.1 $(mandir)/man1/

uninstall:
	$(OCPBUILD) uninstall bigraph
	rm -f $(prefix)/bin/bigrapher
	rm -f $(mandir)/man1/bigrapher.1

call_conf: bin/arg.ml.in bin/bigrapher.ocp.in Makefile.config.in man/bigrapher.1.in lib/bigraph.ocp.in
	@echo
	@echo "    ERROR: you need to run ./configure."
	@echo
	@exit 1

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
