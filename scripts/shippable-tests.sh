#!/bin/sh

# Compile & run tests 
make clean
./configure
make
make test
