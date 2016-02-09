#!/bin/sh

eval `opam config env`

PREFIX=$(opam config var prefix)
XML_DIR=shippable/codecoverage
BISECT_DIR=shippable/bisect
OBJ=_build
BISECT=$(opam config var bin)/bisect-ppx-report

echo "Cleaning"

ocaml setup.ml -uninstall
ocamlfind remove bigraph
ocaml setup.ml -distclean

echo "Compiling for coverage"

sed -i 's/BuildDepends:/BuildDepends: bisect_ppx,/g' _oasis
touch bin/version.ml
oasis setup

ocaml setup.ml -configure --enable-tests --prefix $PREFIX
ocaml setup.ml -build

echo "Running tests"

ocaml setup.ml -test

echo "Generating coverage reports"

$BISECT bisect*.out -I $OBJ -text $BISECT_DIR/report
$BISECT bisect*.out -I $OBJ -summary-only -text $BISECT_DIR/summary
(cd $OBJ; $BISECT ../bisect*.out -no-folding -html ../$BISECT_DIR/html)

cat $BISECT_DIR/summary
