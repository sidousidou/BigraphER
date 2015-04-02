#!/bin/sh

eval `opam config env`

echo CLEANING ...
ocaml setup.ml -distclean

echo COMPILING ...
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -build
