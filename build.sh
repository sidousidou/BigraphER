#!/bin/bash
#ocamlbuild -yaccflags -v bigrapher.native
#ocamlbuild Bigrapher.docdir/index.html
#ocamlbuild -use-ocamlfind -lflags "-cclib -lstdc++","-cclib -lminisat" test_match.p.native
#ocamlbuild -use-ocamlfind -lflags "-cclib -lstdc++","-cclib -L/usr/local/lib/","-cclib -lminisat" test_match.native
#ocamlbuild -use-ocamlfind -cflag "-annot" -lflags "-cclib -lstdc++","-cclib -L/usr/local/lib/","-cclib -lminisat" test_brs.d.byte
ocamlbuild -j 5 -use-ocamlfind -lflags "-cclib -lstdc++","-cclib -L/usr/local/lib/","-cclib -lminisat" test_brs.native
