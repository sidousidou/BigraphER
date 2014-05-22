#!/bin/sh

eval `opam config -env`

# Compile & run tests 
make clean
./configure
make
make test
