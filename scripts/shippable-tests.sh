#!/bin/sh
# Run tests 

eval `opam config env`

echo RUNNING TESTS ... 
ocaml setup.ml -test
