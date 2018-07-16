.PHONY: build release install uninstall clean test doc dist

build:
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f shippable/testresults/*.xml
	rm -f *.tar.gz

test:
	dune runtest

doc:
	dune build @doc

ARCH = bigraph-1.3.0.tar.gz

dist:
	git archive --format=tar --prefix="bigraph/" HEAD | gzip -n > $(ARCH)

