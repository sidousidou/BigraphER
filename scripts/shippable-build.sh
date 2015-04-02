#!/bin/sh

eval `opam config env`

PREFIX=$(opam config var prefix)

echo CLEANING ...
ocaml setup.ml -uninstall
ocamlfind remove bigraph
ocaml setup.ml -distclean

echo COMPILING ...
ocaml setup.ml -configure --enable-tests --prefix $PREFIX
ocaml setup.ml -build
