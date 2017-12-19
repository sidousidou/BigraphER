.PHONY: build release install uninstall clean test doc

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
