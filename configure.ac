AC_INIT(bigrapher,0.4.3)
AC_COPYRIGHT(Copyright 2012-2013 Michele Sevegnani)

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

# Check that OCaml version is greater or equal to 3.12.1
AS_IF([test "x${enable_version_check}" != "xno"], [
  AX_COMPARE_VERSION(
    [$OCAMLVERSION], [lt], [3.12.1],
    AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
])

AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
   AC_MSG_ERROR([You must install the OCaml lexer (the ocamllex command)])
fi

AC_PROG_OCAMLYACC
if test "$OCAMLYACC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml parser (the ocamlyacc command)])
fi

AC_PROG_OCPBUILD
if test "$OCPBUILD" = "no"; then
   AC_MSG_ERROR([You must install ocp-build])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG([minisat])
 if test "$OCAML_PKG_minisat" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'minisat'])
 fi

AC_CHECK_PROG(DOT, dot, "yes")

AC_CONFIG_FILES(
  Makefile.config
  lib/bigraph.ocp
  bin/bigrapher.ocp
  bin/cmd.ml
  man/bigrapher.1
  opam/url
)
AC_OUTPUT

if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi

bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
OCPBUILD="`eval echo ${OCPBUILD}`"

echo
echo Executables will be installed in ${bindir}
echo Manual pages will be installed in ${mandir}
