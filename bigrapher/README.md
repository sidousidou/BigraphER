# BigraphER - Bigraph Evaluator & Rewriting #

Version 1.0.0

[![Build Status](https://api.shippable.com/projects/540f670b21c97efdb898a046/badge?branchName=master)](https://app.shippable.com/projects/540f670b21c97efdb898a046/builds/latest)

----------------------------------------------------------------------------

BigraphER is an implementation of [Bigraphical Reactive System (BRS)][milner]
that supports [bigraphs with sharing][share], stochastic reaction rules, rule
priorities and functional rules. It consists of:

* `bigraph`, an OCaml library to programmatically manipulate
  bigraphs and BRSs, and
* `bigrapher`, a command-line tool to compute the transition system
  of a BRS and export it to the stochastic model checker
  [PRISM](http://www.prismmodelchecker.org/) or to graphical form.

To get started, checkout the
[Install](http://hidden/bigrapher.html#inst) and
[Usage](http://hidden/bigrapher.html#tool) guides.

BigraphER is created and maintained by [Hidden](http://hidden).

## Compiling this repo

* Make sure you have OCaml >= 4.02.0, ocamlfind, minisat (obtainable
  [here](http://hidden/camlminisat.html)), and optionally
  [Graphviz](http://www.graphviz.org/) to support graphical output.
* Run `ocaml setup.ml -configure`.
* Run `ocaml setup.ml -build`.
* Run `ocaml setup.ml -install`.
	    
## Documentation

#### User manual

The main documentation entry point to BigraphER is the man page. It can be
viewed with command `man man/bigrapher.1`.  A short description of the command
line interface is available using `bigrapher --help`.

#### Guides and tutorials

A tutorial is available online at <http://hidden/bigrapher.html>.

#### API documentation

The API documentation can be generated with command `ocaml setup.ml -doc`. It
will be available under `bigraph_api.docdir/`.

## OPAM repository

- [opam-repository] is the repository of the Hidden for the
  BigraphER related [OPAM](http://opam.ocaml.org/) packages. Run the following
  command to add this repository to OPAM: `opam repository add bigrepo
  'http://hidden/'`. The latest release
  of BigraphER can then be installed by running command `opam install
  bigrapher`. OPAM will take care of downloading and installing all the
  dependencies.

## References

- [Milner, Robin. The space and motion of communicating agents. Cambridge
  University Press, 2009.][milner]
- [Sevegnani, Michele, and Muffy Calder. "Bigraphs with sharing." Theoretical
  Computer Science 577 (2015): 43-73.][share]

[milner]: http://dl.acm.org/citation.cfm?id=1540607 "Milner, Robin. The space and motion of communicating agents. Cambridge University Press, 2009."
[share]: http://dx.doi.org/10.1016/j.tcs.2015.02.011 "Sevegnani, Michele, and Muffy Calder. "Bigraphs with sharing." Theoretical Computer Science 577 (2015): 43-73."

## Copyright and license

Copyright 2012-2015 Hidden

All rights reserved. BigraphER is distributed under the terms of the Simplified
BSD License that can be found in the [LICENSE file](LICENSE.md).
