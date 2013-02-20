#!/bin/bash
#ocamlbuild -yaccflags -v bigrapher.native
#ocamlbuild Bigrapher.docdir/index.html
#ocamlbuild -use-ocamlfind -lflags "-cclib -lstdc++","-cclib -lminisat" test_match.p.native
ocamlbuild -use-ocamlfind -lflags "-cclib -lstdc++","-cclib -lminisat" test_match.native