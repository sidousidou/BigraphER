# Bigraph - OCaml library for the manipulation of bigraphs and Bigraphical Reactive Systems

Version 1.3.3

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele),
         Blair Archibald,
         Paulius Dilkas

----------------------------------------------------------------------------

Bigraph is a library to programmaticaly manipulate bigraphs, reaction rules and
Bigraphical Reactive Systems (BRS). It supports both bigraphs and bigraphs with
sharing, probabilistic and stochastic reaction rules, rule priorities, rules
with instantiation maps, parameterised controls, simulation, exhaustive state
space exploration, export to probabilistic model checker [PRISM][prism], and
predicate checking. It is based on an efficient matching engine based on SAT.

### Documentation

The documentation of the library can be accessed at
<http://www.dcs.gla.ac.uk/~michele/docs/bigraph/index.html>.

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
