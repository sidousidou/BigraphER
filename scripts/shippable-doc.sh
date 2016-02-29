#! /usr/bin/env sh

eval `opam config env`

MAN_DIR=shippable/man
API_DIR=shippable/api

echo "Generating documentation"

ocaml setup.ml -doc

mv bigraph_api.docdir/* $API_DIR

groff -mandoc -Thtml man/bigrapher.1 > $MAN_DIR/bigrapher.html
groff -mandoc -Thtml man/bigrapher-full.1 > $MAN_DIR/bigrapher-full.html
groff -mandoc -Thtml man/bigrapher-sim.1 > $MAN_DIR/bigrapher-sim.html
groff -mandoc -Thtml man/bigrapher-validate.1 > $MAN_DIR/bigrapher-validate.html
