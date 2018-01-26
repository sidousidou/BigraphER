# Bigraph - OCaml library for the manipulation of bigraphs and Bigraphical Reactive Systems 

Version 1.0.0

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele)

[![Run Status](https://api.shippable.com/projects/5a393d35fa6ab10700435cb8/badge?branch=master)](https://app.shippable.com/bitbucket/mseve/bigraph)

----------------------------------------------------------------------------

Bigraph is a library to programmaticaly manipulate bigraphs, reaction rules and
Bigraphical Reactive Systems (BRS). It supports both bigraphs and bigraphs with
sharing, probabilistic and stochastic reaction rules, rule priorities, rules
with instantiation maps, parameterised controls, simulation, exhaustive state
space exploration, export to probabilistic model checker [PRISM][prism], and
predicate checking. It is based on an efficient matching engine based on SAT.

### Install

[OPAM][opam] is a source-based package manager for OCaml. It supports all the
major Linux distributions, macOS, BSD systems and Windows (Cygwin). Once OPAM is
installed on your system, add the repository of the University of Glasgow with
the following command:

```
opam repository add glasgow 'http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'
```

Then, to install Bigraph simply run:

```
opam install bigraph
```

OPAM will automatically download and install all the dependencies.

### Documentation

The documentation of the library can be accessed at
<http://www.dcs.gla.ac.uk/~michele/docs/_doc/bigraph>.

## References

- [Robin Milner. *The space and motion of communicating agents*. Cambridge
  University Press (2009).][milner]  
- [Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical
  Computer Science 577 (2015): 43-73.][share]  
- [OPAM: OCaml package manager][opam]
- [PRISM: probabilistic model checker][prism]

[milner]:  <http://dl.acm.org/citation.cfm?id=1540607>
           "Robin Milner. The space and motion of communicating agents. Cambridge University Press (2009)."
[share]:   <http://doi.org/10.1016/j.tcs.2015.02.011>
           "Michele Sevegnani and Muffy Calder. Bigraphs with sharing. Theoretical Computer Science 577 (2015): 43-73."
[opam]:    <http://opam.ocaml.org/> "OPAM: OCaml package manager"
[prism]:   <http://www.prismmodelchecker.org/> "PRISM: probabilistic model checker"
## Copyright and license

Copyright 2018 Michele Sevegnani

All rights reserved. Bigraph is distributed under the BSD-2 license that can be
found in the [LICENSE file](LICENSE.md).
