#! /usr/bin/env sh

OUTDIR=prof

# Recompile

ocaml setup.ml -configure --enable-debug --enable-profile
ocaml setup.ml -build

# Run

./bigrapher.native full -q -M 1000 ./examples/savannah-general.big

MACHINE=$(uname -m)
ARCH=$(uname)
SHA=$(git rev-parse HEAD)

mkdir -p $OUTDIR

# Profile

gprof bigrapher.native | head -n 30 > $OUTDIR/$SHA-$MACHINE-$ARCH
