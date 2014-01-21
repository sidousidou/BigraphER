-include Makefile.config

INSTALL = install
OCPBUILD ?= ocp-build

.PHONY: all install clean distclean test uninstall emacs

all:
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

IN_FILES =  Makefile.config lib/bigraph.ocp bin/bigrapher.ocp 
IN_FILES += bin/cmd.ml man/bigrapher.1 opam/url

distclean:
	$(MAKE) clean 
	rm -f ocp-build.root*
	rm -f config.status config.log
	rm -f $(IN_FILES)

test: _obuild/bigrapher/bigrapher.asm
	$(OCPBUILD) -tests

EMACS ?= emacs

big-mode/big-mode.elc: big-mode/big-mode.el
	$(EMACS) --batch --no-init-file -f batch-byte-compile $<

emacs: big-mode/big-mode.elc
	mkdir -p $(prefix)/share/bigrapher
	$(INSTALL) -m 644 big-mode/big-mode.el $(prefix)/share/bigrapher
	$(INSTALL) -m 644 big-mode/big-mode.elc $(prefix)/share/bigrapher
	@echo "Please add in ~/.emacs.d/init.el or ~/.emacs the following lines:"
	@echo "    (add-to-list 'load-path \"$(prefix)/share/bigrapher/\")"
	@echo "    (require 'big-mode)"

install: all
	$(OCPBUILD) install bigraph	
	$(INSTALL) -m 755 _obuild/bigrapher/bigrapher.asm $(bindir)/bigrapher
	mkdir -p $(mandir)/man1/
	$(INSTALL) -m 644 man/bigrapher.1 $(mandir)/man1/
	$(MAKE) emacs

uninstall:
	$(OCPBUILD) uninstall bigraph
	rm -f $(bindir)/bigrapher
	rm -f $(mandir)/man1/bigrapher.1
	rm -fr $(prefix)/share/bigrapher

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf

.PHONY: prepare-archive complete-archive dist

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

REPO = ~/dcs-opam-repository/packages
REPO_DIR = $(REPO)/bigrapher/bigrapher.$(version)

.PHONY: opam release

opam:
	@echo "    Copying files to local Opam repo ..."
	mkdir -p $(REPO_DIR)
	cp -f opam/opam $(REPO_DIR)
	cp -f opam/descr $(REPO_DIR)
	cp -f opam/url $(REPO_DIR)

upload: $(ARCH_TARGZ)
	@echo "    Uploading archive ..."
	scp $< michele@www.dcs.gla.ac.uk:~/public_html/arch/

COMMIT_MSG = "Add bigrapher.$(version)"
export COMMIT_MSG

release:
	git tag -a "v$(version)" -m "Release $(version)"
	git push --tags
	$(MAKE) upload
	$(MAKE) opam
	$(MAKE) -C $(REPO) release
