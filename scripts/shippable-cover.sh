#! /usr/bin/env sh -ex

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

$BISECT -I $OBJ -text $BISECT_DIR/report bisect*.out 
$BISECT -I $OBJ -summary-only -text $BISECT_DIR/summary bisect*.out 
$BISECT -I $OBJ -html $BISECT_DIR/html bisect*.out

cat $BISECT_DIR/summary

./cobertura.native $BISECT_DIR/report $XML_DIR coverage.xml $COMMIT


