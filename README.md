# Big_json - JSON codec for bigraphical structures

Version 0.2.0

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele), Paulius Dilkas

----------------------------------------------------------------------------

Big_json is a codec to encode and decode bigraphical structures such as bigraphs
and reaction rules to the [JSON][json] data format. The library provides a JSON
interface to the matching engine of the [bigraph][big_lib] library. This is
also exposed by the `big_match` command line tool.

### Install

[OPAM][opam] is a source-based package manager for OCaml. It supports all the
major Linux distributions, macOS, BSD systems and Windows (Cygwin). Once OPAM is
installed on your system, add the repository of the University of Glasgow with
the following command:

```
opam repository add glasgow 'http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'
```

Then, to install Big_json simply run:

```
opam install big_json
```

OPAM will automatically download and install all the dependencies.

### Documentation

The documentation of the library can be accessed at
<http://www.dcs.gla.ac.uk/~michele/docs/_doc/big_json>.

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

## Copyright and license

Copyright 2018 Michele Sevegnani

All rights reserved. Big_json is distributed under the BSD-2 license that can be
found in the [LICENSE file](LICENSE.md).
