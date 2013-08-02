#!/usr/bin/env bash

set -e

VERSION=0.3.0
NAME=bigraph
BINNAME=bigrapher

LIB=lib.otarget
TEST=test.otarget
BIN=bigrapher.byte
BINOPT=bigrapher.native

OCAMLBUILD=ocamlbuild
OCBFLAGS="-use-ocamlfind -j 0 -verbose 1 -yaccflags -v"

if [ `uname -o` = "Cygwin" ]; then
    OCAMLFIND="ocamlfind"
else
    OCAMLFIND="sudo ocamlfind"
fi

DLLPATH=`ocamlfind query minisat`
OCAMLCFLAGS='-verbose -noassert "-dllpath $DLLPATH" "-dllib -lminisat" "-ccopt -O3"'
OCAMLC="ocamlc $OCAMLCFLAGS"
OCAMLOPTFLAGS='-verbose -noassert "-inline 2" -ffast-math "-ccopt -O3"'
OCAMLOPT="ocamlopt $OCAMLOPTFLAGS"

INSTDIR=/usr/bin/

ocb() {
#    if [ `uname -o` = "Cygwin" ]; then
#	$OCAMLBUILD $OCBFLAGS -ocamlc '$OCAMLC' -ocamlopt '$OCAMLOPT' $*
# Still need ocamlrun -I $DLLPATH *.byte
#    else
	$OCAMLBUILD $OCBFLAGS $*
#    fi
}

install() {
    ocb $LIB #$BINOPT
    MLI=`ls ./_build/lib/*.mli`
    CMI=`ls ./_build/lib/*.cmi`
    #CMA=`ls ./_build/lib/*.cma`
    #CMXA=`ls ./_build/lib/*.cmxa`
    DLL=`ls ./_build/lib/*a`
    INSTALLFILES="./lib/META $MLI $CMI $DLL"
    $OCAMLFIND remove $NAME || true
    #rm $INSTDIR/$BINNAME  || true
    $OCAMLFIND install -patch-version $VERSION $NAME $INSTALLFILES
#    install -m 755 _build/front-end/$BINOPT $INSTDIR/$BINNAME
}

uninstall() {
    $OCAMLFIND remove $NAME
    #rm $INSTDIR/$BINNAME
}

dist() {
    echo "Not yet"
}

rule() {
    case $1 in
	clean) ocb -clean;;
	lib) ocb $LIB;;   
	test) ocb -clean
	    ocb $TEST;;
	bin) ocb $BIN $BINOPT;;
	all) ocb $LIB $TEST $BIN $BINOPT;;
	dist) dist;;
	install) install;;
	uninstall) uninstall;;
	*) echo "Unknown action $1";;
    esac;
}

if [ $# -eq 0 ]; then
    rule all
else
    while [ $# -gt 0 ]; do
	rule $1;
	shift
    done
fi
