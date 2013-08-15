#!/usr/bin/env sh

set -e

: ${DEBUG:=false}

VERSION=0.3.0
NAME=bigraph
BINNAME=bigrapher

LIB=lib.otarget
TEST=test.otarget
BIN=bigrapher.byte

if [ "$DEBUG" = "false" ]; then
    BINOPT=bigrapher.native
else
    BINOPT=bigrapher.p.native
fi


OCAMLBUILD=ocamlbuild
OCBFLAGS="-use-ocamlfind -j 0 -verbose 1 -yaccflags -v"
OCAMLFIND=ocamlfind

DLLPATH=`ocamlfind query minisat`

#OCAMLCFLAGS='-verbose -noassert "-dllpath $DLLPATH" "-dllib -lminisat" "-ccopt -O3" -ccopt "-march=native"'
#OCAMLC="ocamlc $OCAMLCFLAGS"
#OCAMLOPTFLAGS='-verbose -noassert "-inline 2" -ffast-math "-ccopt -O3" "-ccopt -march=native"'
#OCAMLOPT="ocamlopt $OCAMLOPTFLAGS"

: ${INSTDIR:=/usr/bin/}

DISTBIG="`ls bin/*.ml` `ls bin/*.mll` `ls bin/*.mly` bin/_tags"
DISTLIB="`ls lib/*.ml` `ls lib/*.mli` lib/bigraph.mllib lib/_tags lib/lib.itarget lib/bigraph.odocl lib/META"
DISTTEST="`ls tests/*.ml` tests/_tags tests/test.itarget"
DISTTEST="$DISTTEST tests/files/rts_cts.big `ls tests/files/match/*.big`"
DISTSRC="build.sh INSTALL LICENSE README"
DISTSRC="$DISTBIG $DISTLIB $DISTTEST $DISTSRC"

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
	$OCAMLBUILD $OCBFLAGS $*
}

lib_install_rule() {
    printf 'Installing library.\n'
    MLI=`ls lib/_build/*.mli` || true 
    CMI=`ls lib/_build/*.cmi` || true
    DLL=`ls lib/_build/*a` || true
    #check if the files are there
    if [ "$MLI$CMI$DLL" = "" ]; then
	printf 'Error: Library not compiled.\nTry running ./build lib first.\n' 2> /dev/null
	exit 1
    else
	INSTALLFILES="lib/META $MLI $CMI $DLL"
	$OCAMLFIND remove $NAME || true
     	$OCAMLFIND install -patch-version $VERSION $NAME $INSTALLFILES
    fi
}

bin_install_rule() {
    printf 'Installing bigrapher in %s.\n' $INSTDIR
    if [ -e "bin/_build/$BINOPT" ]; then
	if [ "$OSTYPE" = "cygwin" ]; then
            install -m 755 bin/_build/$BINOPT "$INSTDIR$BINNAME.exe"
            #add install dir of bigraph to PATH in Cygwin
	else
	    install -m 755 bin/_build/$BINOPT $INSTDIR$BINNAME
 	fi
    else
	printf 'Error: bigrapher not compiled.\nTry running ./build bin first.\n' 2> /dev/null
	exit 1
    fi
}

uninstall_rule() {
    $OCAMLFIND remove $NAME
    if [ "$OSTYPE" = "cygwin" ]; then
	rm "$INSTDIR$BINNAME.exe"
    else
	rm "$INSTDIR$BINNAME"
    fi
}

dist() {
    printf "Preparing files...\n"
    TARSRC=""
    for f in $DISTSRC; do
	TARSRC="$TARSRC `printf "$BINNAME-$VERSION/%s" $f`"
    done
    cd ..
    cp -R $BINNAME "$BINNAME-$VERSION"
    tar zcvf "$BINNAME-$VERSION.tar.gz" $TARSRC
    rm -Rf "$BINNAME-$VERSION"
    cd $BINNAME
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
    cd bin
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
    cd ../bin
    ocb -clean
    echo ""
    rm -f *.byte *.cm*a *.a *.native || true
    cd ..
}

all_rule(){
    lib_rule
    lib_install_rule
    bin_rule
    test_rule
}

install_rule(){
    lib_rule
    lib_install_rule
    bin_rule
    bin_install_rule
}

rule() {
    case $1 in
	clean) clean_rule;;
	lib) lib_rule;;   
	test) test_rule;;
	bin) bin_rule;;
	all) all_rule;;
	dist) dist;;
	install) install_rule;;
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
