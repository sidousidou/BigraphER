#!/bin/sh

# OPAM DCS repository
export DCS_REPO='http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'

# OPAM packages needed to build tests
export OPAM_PACKAGES='ocp-build minisat'

# Install packages from OPAM
opam init -q -a --sh
eval `opam config -env`
opam repository -q -y add dcs ${DCS_REPO}
opam install -q -y ${OPAM_PACKAGES}
