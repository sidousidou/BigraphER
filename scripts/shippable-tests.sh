#!/bin/sh

eval `opam config -env`

# Compile & run tests 
make distclean
./configure
make
make test
