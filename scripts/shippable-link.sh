#!/bin/sh

eval `opam config env`

echo INSTALLING BIGRAPHER
ocp-build install bigraph

echo BYTE-CODE COMPILATION TEST
ocamlfind ocamlc -o ./shippable/ex.byte -package bigraph -linkpkg ./examples/ex.ml

echo NATIVE-CODE COMPILATION TEST
ocamlfind opt -o ./shippable/ex.asm -package bigraph -linkpkg ./examples/ex.ml
