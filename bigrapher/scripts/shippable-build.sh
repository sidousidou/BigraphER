#! /usr/bin/env sh

eval `opam config env`

PREFIX=$(opam config var prefix)

echo "Build for release"

ocaml setup.ml -uninstall
ocaml setup.ml -distclean

ocaml setup.ml -configure --prefix $PREFIX
ocaml setup.ml -build
