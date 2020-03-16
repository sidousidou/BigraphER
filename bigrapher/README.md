# BigraphER - Bigraph Evaluator & Rewriting #

Version 1.9.2

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele), [Blair Archibald](http://www.blairarchibald.co.uk)

[![Run Status](https://api.shippable.com/projects/540f670b21c97efdb898a046/badge?branch=master)](https://app.shippable.com/bitbucket/mseve/bigrapher)

----------------------------------------------------------------------------

BigraphER is a command-line tool to compute the transition system of a BRS and
export it to the stochastic model checker [PRISM](prism) or to graphical form.

To get started, checkout the
[Install](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#inst) and
[Usage](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#tool) guides.

### Install

[OPAM][opam] is a source-based package manager for OCaml. It supports all the
major Linux distributions, macOS, BSD systems and Windows (Cygwin). Once OPAM is
installed on your system, add the repository of the University of Glasgow with
the following command:

```
opam repository add glasgow 'http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'
```

Then, to install BigraphER simply run:

```
opam install bigrapher
```

OPAM will automatically download and install all the dependencies.
	    
### Documentation

#### User manual

The main documentation entry point to BigraphER is the man page. It can be
viewed with command `man man/bigrapher.1`.  A short description of the command
line interface is available using `bigrapher --help`.

#### Guides and tutorials

A tutorial is available online at
<http://www.dcs.gla.ac.uk/~michele/bigrapher.html>.  A more extensive guide is
available in PDF format as [CAV 2016 tool paper][tech].

### References

- [Robin Milner. *The space and motion of communicating agents*. Cambridge
  University Press (2009).][milner]
- [Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical
  Computer Science 577 (2015): 43-73.][share]
- [Michele Sevegnani and Muffy Calder. *BigraphER: rewriting and analysis engine for bigraphs*. Proceedings of Computer Aided Verification (CAV 2016), Lecture Notes in Computer Science 9780 (2016): 494-501.][tech]
- [OPAM: OCaml package manager][opam]
- [PRISM: probabilistic model checker][prism]

[milner]: <http://dl.acm.org/citation.cfm?id=1540607> "Robin Milner. *The space and motion of communicating agents*. Cambridge University Press (2009)."
[share]: <http://doi.org/10.1016/j.tcs.2015.02.011> "Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical Computer Science 577 (2015): 43-73."
[tech]: <http://doi.org/10.1007/978-3-319-41540-6_27> "Michele Sevegnani and Muffy Calder. *BigraphER: rewriting and analysis engine for bigraphs*. Proceedings of Computer Aided Verification (CAV 2016), Lecture Notes in Computer Science 9780 (2016): 494-501"
[opam]:    <http://opam.ocaml.org/> "OPAM: OCaml package manager"
[prism]:   <http://www.prismmodelchecker.org/> "PRISM: probabilistic model checker"

### Copyright and license

Copyright 2012-2019 Michele Sevegnani

All rights reserved. BigraphER is distributed under the terms of the Simplified
BSD License that can be found in the [LICENSE file](LICENSE.md).
