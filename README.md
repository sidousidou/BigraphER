# Bigraph-Tools #

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele), [Blair Archibald](http://www.blairarchibald.co.uk)

[![Run Status](https://api.shippable.com/projects/5e6fa6aa5f10de0007bc21a1/badge?branch=master)]()
[![Docker Cloud Build Status](https://img.shields.io/docker/cloud/build/mseve/bigrapher)]()

This repository contains a collection of tools for writing and
anlysing Bigraph models. It contains:

1. *bigraph* - a library to programmaticaly manipulate bigraphs, reaction rules
  and Bigraphical Reactive Systems (BRS). It supports both bigraphs and bigraphs
  with sharing, probabilistic and stochastic reaction rules, rule priorities,
  rules with instantiation maps, parameterised controls, simulation, exhaustive
  state space exploration, export to probabilistic model checker [PRISM][prism],
  and predicate checking. It is based on an efficient matching engine based on
  SAT.

2. *bigrapher* - BigraphER is a command-line tool to compute the transition
   system of a BRS and export it to the stochastic model checker [PRISM](prism)
   or to graphical form.

3. *big_json* - Big_json is a codec to encode and decode bigraphical
  structures such as bigraphs and reaction rules to the [JSON](json) data
  format. The library provides a JSON interface to the matching engine of the
  *bigraph* library. This is also exposed by the `big_match` command line tool.

4. *minisat* - Minisat provides bindings to the [MiniSAT](minisat) SAT solver.

5. *minicard* - Minicard provides bindings to the [MiniCARD](minicard)
  cardinality solver.

----------------------------------------------------------------------------

### Building the Tools

The tools can be built using dune (min version 2) as follows:

```
dune build --profile=release
```

To install the tools to a local opam repository you can use:

```
dune install --profile=release
```

## References

- [Robin Milner. *The space and motion of communicating agents*. Cambridge
  University Press (2009).][milner]
- [Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical
  Computer Science 577 (2015): 43-73.][share]
- [Michele Sevegnani and Muffy Calder. *BigraphER: rewriting and analysis engine for bigraphs*. Proceedings of Computer Aided Verification (CAV 2016), Lecture Notes in Computer Science 9780 (2016): 494-501.][tech]
- [T. Bray Ed. *The JavaScript Object Notation (JSON) Data Interchange Format*. (2014)][json]
- [OPAM: OCaml package manager][opam]
- [PRISM: probabilistic model checker][prism]
- [MiniSAT: a minimalistic and high-performance SAT solver][minisat]
- [MiniCARD cardinality solver][minicard]

[milner]:  <http://dl.acm.org/citation.cfm?id=1540607> "Robin Milner. *The space and motion of communicating agents*. Cambridge University Press (2009)."
[share]:   <http://doi.org/10.1016/j.tcs.2015.02.011> "Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical Computer Science 577 (2015): 43-73."
[tech]:    <http://doi.org/10.1007/978-3-319-41540-6_27> "Michele Sevegnani and Muffy Calder. *BigraphER: rewriting and analysis engine for bigraphs*. Proceedings of Computer Aided Verification (CAV 2016), Lecture Notes in Computer Science 9780 (2016): 494-501"
[json]:    <http://tools.ietf.org/html/rfc7159>
           "T. Bray Ed. The JavaScript Object Notation (JSON) Data Interchange Format, 2014"
[prism]:   <http://www.prismmodelchecker.org/> "PRISM: probabilistic model checker"
[minisat]: <https://github.com/niklasso/minisat> "MiniSAT: a minimalistic and high-performance SAT solver"
[minicard]: <https://github.com/liffiton/minicard> "MiniCARD is a cardinality solver based on MiniSAT"
[opam]:    <https://opam.ocaml.org/> "OPAM: OCaml package manager"


### Copyright and license

Copyright 2012-2020 Glasgow Bigraph Team

All rights reserved. Tools distributed under the terms of the Simplified
BSD License that can be found in the [LICENSE file](LICENSE.md).
