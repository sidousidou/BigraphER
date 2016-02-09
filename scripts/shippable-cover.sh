#!/bin/sh

eval `opam config env`

PREFIX=$(opam config var prefix)
XML_DIR=shippable/codecoverage
BISECT_DIR=shippable/bisect
OBJ=_build

echo "Cleaning"

ocaml setup.ml -uninstall
ocamlfind remove bigraph
ocaml setup.ml -clean

echo "Compiling"

sed -i 's/BuildDepends:/BuildDepends: bisect_ppx,/g' _oasis
touch bin/version.ml
oasis setup

ocaml setup.ml -configure --enable-tests --prefix $PREFIX
ocaml setup.ml -build

echo "Running tests"

ocaml setup.ml -test

echo "Generating coverage reports"

bisect-report bisect*.out -I $OBJ -text $BISECT_DIR/report
bisect-report bisect*.out -I $OBJ -summary-only -text $BISECT_DIR/summary
(cd $OBJ;
 bisect-report ../bisect*.out -html ../$BIDECT_DIR/html;
 bisect-report ../bisect*.out -xml ../$XML_DIR/report.xml;
 bisect-report ../bisect*.out -xml-emma ../$XML_DIR/report-emma.xml)

cat $BISECT_DIR/summary
