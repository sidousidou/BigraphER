#!/bin/sh

# OCaml version to install
export OCAML_VERSION='4.01.0'

# OPAM version to install
export OPAM_VERSION='1.1.1'

# OPAM DCS repository
export DCS_REPO='http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'

# OPAM packages needed to build tests
export OPAM_PACKAGES='ocp-build minisat'

# Install OCaml
sudo apt-get update -qq
sudo apt-get install -qq ocaml

echo 'OCaml version'
ocaml -version

# Install OPAM
curl -L https://github.com/ocaml/opam/archive/${OPAM_VERSION}.tar.gz | tar xz -C /tmp
pushd /tmp/opam-${OPAM_VERSION}
./configure
make
sudo make install
opam init
eval `opam config -env`
popd

# Install packages from OPAM
#opam switch ${OCAML_VERSION}  
opam repository add dcs ${DCS_REPO}
opam install -q -y ${OPAM_PACKAGES}

# Compile & run tests 
./configure
make test
