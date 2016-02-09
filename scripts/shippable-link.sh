#!/bin/sh

ROOT=./shippable/linktest
TEST=./shippable/testresults
SRC=./tests/files/link

eval `opam config env`

ocaml setup.ml -install
ocaml setup.ml -distclean

cp ./examples/ex.ml $ROOT/ex.ml

echo "Linking bytecode"

ocamlfind ocamlc -o $ROOT/ex.byte -package bigraph -linkpkg $ROOT/ex.ml
if $ROOT/ex.byte | cmp -s -  $SRC/ex.reference &> /dev/null
  then cp $SRC/byte-0.xml $TEST/byte-link.xml
  else cp $SRC/byte-1.xml $TEST/byte-link.xml
fi

echo "Linking native code"

ocamlfind ocamlopt -o $ROOT/ex.native -package bigraph -linkpkg $ROOT/ex.ml
if $ROOT/ex.native | cmp -s -  $SRC/ex.reference &> /dev/null
  then cp $SRC/native-0.xml $TEST/native-link.xml
  else cp $SRC/native-1.xml $TEST/native-link.xml
fi
