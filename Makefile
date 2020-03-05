.PHONY: build release uninstall clean test doc dist emacs-mode dist man

build:
	dune build @install

install: emacs-mode man
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f shippable/testresults/*.xml
	rm -f *.tar.gz

test:
	mkdir -p shippable/testresults/
	dune runtest

doc:
	dune build @doc

ARCH = bigrapher-1.9.1.tar.gz

dist:
	git archive --format=tar --prefix="bigrapher/" HEAD | gzip -n > $(ARCH)

EMACS ?= emacs

emacs-mode: big-mode/big-mode.el
	$(EMACS) --batch --no-init-file -f batch-byte-compile $<

man: man/bigrapher.1 man/bigrapher-full.1 man/bigrapher-sim.1 man/bigrapher-validate.1
	mandoc -mandoc -T html -O style=mandoc.css,man=%N.html man/bigrapher.1 > man/bigrapher.html
	mandoc -mandoc -T html -O style=mandoc.css,man=%N.html man/bigrapher-full.1 > man/bigrapher-full.html
	mandoc -mandoc -T html -O style=mandoc.css,man=%N.html man/bigrapher-sim.1 > man/bigrapher-sim.html
	mandoc -mandoc -T html -O style=mandoc.css,man=%N.html man/bigrapher-validate.1 > man/bigrapher-validate.html
