ocamlfind ocamlc -o ../shippable/ex.byte -package bigraph -linkpkg ../examples/ex.ml
ocamlfind opt -o ../shippable/ex.asm -package bigraph -linkpkg ../examples/ex.ml
