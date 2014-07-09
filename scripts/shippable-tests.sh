#!/bin/sh
# Compile & run tests 

eval `opam config -env`

echo CLEANING ...
ocp-build uninstall bigraph
make distclean

echo COMPILING ...
./configure
make

echo RUNNING TESTS ... 
make test
