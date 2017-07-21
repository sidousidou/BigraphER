#! /usr/bin/env sh

eval `opam config env`

API_DIR=shippable/api

echo "Generating documentation"

ocaml setup.ml -doc

mv bigraph_api.docdir/* $API_DIR
