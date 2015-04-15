# BigraphER - Bigraph Evaluator & Rewriting #

Version 0.6.0   
[![Build Status](https://api.shippable.com/projects/540f670b21c97efdb898a046/badge?branchName=master)](https://app.shippable.com/projects/540f670b21c97efdb898a046/builds/latest)

----------------------------------------------------------------------------

BigraphER is an implementation of [Bigraphical Reactive System
(BRS)][milner] that supports [bigraphs with sharing][share], stochastic reaction rules, rule
priorities and functional rules. It constist of:

*  OCaml library *bigraph* to programmatically manipulate bigraphs and BRSs, 
*  the command-line tool *bigrapher* to compute the transition system of a BRS and export
   it to the stochastic model checker [PRISM](http://www.prismmodelchecker.org/) or to
   graphical form.

To get started, checkout the [Install](http://install) and [Usage](http://usage) guides.

Bigrapher is created and maintained by [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele).

## Compiling this repo

* Make sure you have OCaml >= 4.02.0, ocamlfind, minisat (obtainable
  [here](http://www.dcs.gla.ac.uk/~michele/camlminisat.html)), and optionally
  [Graphviz](http://www.graphviz.org/) to produce graphical output.
* Run `ocaml setup.ml -configure`.
* Run `ocaml setup.ml -build`.
* Run `ocaml setup.ml -install`.
	    
## Documentation

#### User manual

The main documentation entry point to BigraphER is the man page.
A short description of the command line interface is available using `bigrapher --help`.

#### Guides and tutorials

A tutorial is available online at <http://www.dcs.gla.ac.uk/~michele/bigrapher.html>.
A more extensive guide is available as [technical report][tech].

#### API, code documentation and developer manual

The API documentation can be generated with command `ocaml setup.ml -doc`. It will be
available under `bigraph_api.docdir/`.

## OPAM repository

- [dcs-opam-repository](http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/) is the
  repository for the BigraphER related [OPAM](http://opam.ocaml.org/) packages.
  
## References

[milner]: http://milner "Milner book"
[share]: http://tcs "Bigraphs with sharing"
[tech]: http://tech "BigraphER: for bigraphs"

## Copyright and license

Copyright 2012-2015 Michele Sevegnani

All rights reserved. BigraphER is distributed under the terms of the Simplified BSD License.
