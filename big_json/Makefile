.PHONY: build release install uninstall clean test doc dist

build:
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f *.tar.gz

test:
	dune runtest

doc:
	dune build @doc

ARCH = big_json-0.2.1.tar.gz

dist:
	git archive --format=tar --prefix="bigraph/" HEAD | gzip -n > $(ARCH)
