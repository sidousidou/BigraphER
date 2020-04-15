# Big_json - JSON codec for bigraphical structures

Version 0.2.1

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele), Paulius Dilkas

----------------------------------------------------------------------------

Big_json is a codec to encode and decode bigraphical structures such as bigraphs
and reaction rules to the [JSON][json] data format. The library provides a JSON
interface to the matching engine of the [bigraph][big_lib] library. This is
also exposed by the `big_match` command line tool.

### Documentation

The documentation of the library can be accessed at
<http://www.dcs.gla.ac.uk/~michele/docs/big_json/index.html>.

## References

- [Robin Milner. *The space and motion of communicating agents*. Cambridge
  University Press (2009).][milner]
- [Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical
  Computer Science 577 (2015): 43-73.][share]
- [T. Bray Ed. *The JavaScript Object Notation (JSON) Data Interchange Format*. (2014)][json]
- [OPAM: OCaml package manager][opam]
- [Bigraph: an OCaml library to manipulate bigraphs and Bigraphical Reactive Systems][big_lib]

[milner]:  <http://dl.acm.org/citation.cfm?id=1540607>
           "Robin Milner. The space and motion of communicating agents. Cambridge University Press (2009)."
[share]:   <http://doi.org/10.1016/j.tcs.2015.02.011>
           "Michele Sevegnani and Muffy Calder. Bigraphs with sharing. Theoretical Computer Science 577 (2015): 43-73."
[json]:    <http://tools.ietf.org/html/rfc7159>
           "T. Bray Ed. The JavaScript Object Notation (JSON) Data Interchange Format, 2014"
[opam]:    <http://opam.ocaml.org/> "OPAM: OCaml package manager"
[big_lib]: <http://www.dcs.gla.ac.uk/~michele/docs/_doc/bigraph/>
           "Bigraph: an OCaml library to manipulate bigraphs and Bigraphical Reactive Systems"
