.PHONY: build release install uninstall clean test doc dist

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	rm -f shippable/testresults/*.xml

test:
	jbuilder runtest

doc:
	jbuilder build @doc

ARCH = bigraph-1.2.0.tar.gz

dist:
	git archive --format=tar --prefix="bigraph/" HEAD | gzip -n > $(ARCH)

