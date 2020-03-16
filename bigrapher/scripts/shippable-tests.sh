#!/bin/sh
# Run tests 

eval `opam config env`

ocaml setup.ml -test
