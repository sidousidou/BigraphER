# BigraphER - Bigraph Evaluator & Rewriting #

Version 0.6.0

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
[Install](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#inst) and
[Usage](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#tool) guides.

BigraphER is created and maintained by [Michele
Sevegnani](http://www.dcs.gla.ac.uk/~michele).

## Compiling this repo

* Make sure you have OCaml >= 4.02.0, ocamlfind, minisat (obtainable
  [here](http://www.dcs.gla.ac.uk/~michele/camlminisat.html)), and optionally
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

A tutorial is available online at
<http://www.dcs.gla.ac.uk/~michele/bigrapher.html>.  A more extensive guide is
available in PDF format as [technical report][tech].

#### API documentation

The API documentation can be generated with command `ocaml setup.ml -doc`. It
will be available under `bigraph_api.docdir/`.

## OPAM repository

- [dcs-opam-repository] is the repository of the University of Glasgow for the
  BigraphER related [OPAM](http://opam.ocaml.org/) packages. Run the following
  command to add this repository to OPAM: `opam repository add glasgow
  'http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'`

## References

- [Milner, Robin. The space and motion of communicating agents. Cambridge University Press, 2009.][milner]
- [Sevegnani, Michele, and Muffy Calder. "Bigraphs with sharing." Theoretical Computer Science 577 (2015): 43-73.][share]
- [Sevegnani, Michele. BigraphER: rewriting and analysis engine for bigraphs. 2015.][tech]

[milner]: http://dl.acm.org/citation.cfm?id=1540607 "Milner, Robin. The space and motion of communicating agents. Cambridge University Press, 2009."
[share]: http://dx.doi.org/10.1016/j.tcs.2015.02.011 "Sevegnani, Michele, and Muffy Calder. "Bigraphs with sharing." Theoretical Computer Science 577 (2015): 43-73."
[tech]: http://www.dcs.gla.ac.uk/~michele/papers/tech_BigraphER.pdf "Sevegnani, Michele. BigraphER: rewriting and analysis engine for bigraphs. 2015."

## Copyright and license

Copyright 2012-2015 Michele Sevegnani

All rights reserved. BigraphER is distributed under the terms of the
Simplified BSD License that can be found in the [LICENSE file](LICENSE.md).