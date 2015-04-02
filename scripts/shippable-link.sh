#!/bin/sh

ROOT=./shippable

eval `opam config env`

echo INSTALLING BIGRAPH LIBRARY
ocaml setup.ml -install

echo CLEANING ...
ocaml setup.ml -distclean

cp ./examples/ex.ml $ROOT/tmp/ex.ml

echo BYTE-CODE COMPILATION TEST
ocamlfind ocamlc -o $ROOT/ex.byte -package bigraph -linkpkg $ROOT/tmp/ex.ml
$ROOT/ex.byte | cmp -s -  ./tests/files/ex.reference

echo NATIVE-CODE COMPILATION TEST
ocamlfind ocamlopt -o $ROOT/ex.asm -package bigraph -linkpkg $ROOT/tmp/ex.ml
$ROOT/ex.native | cmp -s -  ./tests/files/ex.reference
