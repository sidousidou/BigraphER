# bigraph #

### OCaml library for the manipulation of [Bigraphs](https://en.wikipedia.org/wiki/Bigraph) and Bigraphical Reactive Systems ###

Version 1.0.0

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele)

[![Run Status](https://api.shippable.com/projects/5a393d35fa6ab10700435cb8/badge?branch=master)](https://app.shippable.com/bitbucket/mseve/bigraph)

----------------------------------------------------------------------------

To get started, checkout the
[Install](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#inst) and
[Usage](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#tool) guides.


## Compiling this repo

#### API documentation

## OPAM repository

- [dcs-opam-repository] is the repository of the University of Glasgow for the
  BigraphER related [OPAM](http://opam.ocaml.org/) packages. Run the following
  command to add this repository to OPAM: `opam repository add glasgow
  'http://www.dcs.gla.ac.uk/~michele/dcs-opam-repository/'`. The latest release
  of this library can be installed by running command `opam install
  bigraph`. OPAM will take care of downloading and installing all the
  dependencies.

## References

- [Robin Milner. *The space and motion of communicating agents*. Cambridge
  University Press (2009).][milner]
- [Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical
  Computer Science 577 (2015): 43-73.][share]
- [Michele Sevegnani and Muffy Calder. *BigraphER: rewriting and analysis engine for bigraphs*. Proceedings of Computer Aided Verification (CAV 2016), Lecture Notes in Computer Science 9780 (2016): 494-501.][tech]

[milner]: http://dl.acm.org/citation.cfm?id=1540607 "Robin Milner. *The space and motion of communicating agents*. Cambridge University Press (2009)."
[share]: http://doi.org/10.1016/j.tcs.2015.02.011 "Michele Sevegnani and Muffy Calder. *Bigraphs with sharing*. Theoretical Computer Science 577 (2015): 43-73."
[tech]: http://doi.org/10.1007/978-3-319-41540-6_27 "Michele Sevegnani and Muffy Calder. *BigraphER: rewriting and analysis engine for bigraphs*. Proceedings of Computer Aided Verification (CAV 2016), Lecture Notes in Computer Science 9780 (2016): 494-501"

## Copyright and license

Copyright 2012-2018 Michele Sevegnani

All rights reserved. BigraphER is distributed under the terms of the Simplified
BSD License that can be found in the [LICENSE file](LICENSE.md).
