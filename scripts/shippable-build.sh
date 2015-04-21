#!/bin/sh

eval `opam config env`

PREFIX=$(opam config var prefix)

ocaml setup.ml -uninstall
ocamlfind remove bigraph
ocaml setup.ml -distclean

ocaml setup.ml -configure --enable-tests --prefix $PREFIX
ocaml setup.ml -build
