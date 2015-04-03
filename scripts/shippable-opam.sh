#!/bin/sh

# OPAM DCS repository
DCS_REPO='http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'

# OPAM packages needed to build tests
OPAM_PACKAGES='ocp-build minisat'

# Install packages from OPAM
opam init -q -a -y
eval `opam config env`
opam repository -q -y add dcs $DCS_REPO
opam install -q -y minisat menhir
