#!/usr/bin/env sh

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
OCAMLFIND=ocamlfind

DLLPATH=`ocamlfind query minisat`

#OCAMLCFLAGS='-verbose -noassert "-dllpath $DLLPATH" "-dllib -lminisat" "-ccopt -O3" -ccopt "-march=native"'
#OCAMLC="ocamlc $OCAMLCFLAGS"
#OCAMLOPTFLAGS='-verbose -noassert "-inline 2" -ffast-math "-ccopt -O3" "-ccopt -march=native"'
#OCAMLOPT="ocamlopt $OCAMLOPTFLAGS"

: ${INSTDIR:=/usr/bin/}
OS='uname -o'

ERRMSG="Error: Unknown action $1\nUsage: build OPTION\nThe options are as follows:\n\
clean\t\tRemove outputs of previous compilations\n\
lib\t\tCompile library\n\
test\t\tCompile tests\n\
bin\t\tCompile bigrapher\n\
all\t\tCompile library and bigrapher\n\
dist\t\tProduce an archive for distribution\n\
install\t\tInstall library and bigrapher\n\
uninstall\tRemove library and bigrapher\n"

ocb() {
#    if [ `uname -o` = "Cygwin" ]; then
#	$OCAMLBUILD $OCBFLAGS -ocamlc '$OCAMLC' -ocamlopt '$OCAMLOPT' $*
# Still need ocamlrun -I $DLLPATH *.byte
#    else
	$OCAMLBUILD $OCBFLAGS $*
#    fi
}

lib_install_rule() {
    printf 'Installing library.\n'
    MLI=`ls lib/_build/*.mli` || true 
    CMI=`ls lib/_build/*.cmi` || true
    DLL=`ls lib/_build/*a` || true
    #check if the files are there or not
    if [ "$MLI $CMI $DLL" = "" ]; then
	printf 'Error: Library not compiled.\nTry running ./build lib first.\n' 2> /dev/null
	exit 1
    else
	INSTALLFILES="lib/META $MLI $CMI $DLL"
	$OCAMLFIND remove $NAME || true
     	$OCAMLFIND install -patch-version $VERSION $NAME $INSTALLFILES
  fi
}

bin_install_rule() {
    printf 'Installing bigrapher in $INSTDIR.\n'
    if [ -e "bigrapher/_build/$BINOPT" ]; then
	printf ""
          #install -m 755 bigrapher/_build/$BINOPT $INSTDIR/$BINNAME
          #add install dir of bigraph to PATH in Cygwin
    else
	printf 'Error: bigrapher not compiled.\nTry running ./build bin first.\n' 2> /dev/null
	exit 1
    fi
}

uninstall_rule() {
    $OCAMLFIND remove $NAME
    #rm $INSTDIR/$BINNAME
}

dist() {
    echo "Not yet"
}

lib_rule() {
    cd lib
    ocb $LIB
    cd ..
}

test_rule(){
    cd tests
    ocb $TEST
    cd ..
}

bin_rule(){
    cd bigrapher
    ocb $BIN $BINOPT
    cd ..
}

clean_rule(){
    cd lib 
    ocb -clean
    echo ""
    rm -f *.byte *.cm*a *.a *.native || true
    cd ../tests
    ocb -clean
    echo ""
    rm -f *.byte *.cm*a *.a *.native || true
    cd ../bigrapher
    ocb -clean
    echo ""
    rm -f *.byte *.cm*a *.a *.native || true
    cd ..
}

rule() {
    case $1 in
	clean) clean_rule;;
	lib) lib_rule;;   
	test) test_rule;;
	bin) bin_rule;;
	all) #clean_rule 
	    lib_rule
	    lib_install_rule
	    bin_rule;;
	    #bin_install_rule
	    #clean_rule;;
	dist) dist;;
	install) clean_rule 
	    lib_rule
	    lib_install_rule
	    bin_rule;;
	    #bin_install_rule
	    #clean_rule;;
	uninstall) uninstall_rule;;
	*) printf "$ERRMSG" 2> /dev/null;;
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
