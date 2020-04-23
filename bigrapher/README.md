# BigraphER - Bigraph Evaluator & Rewriting #

Version 1.9.3

Authors: [Michele Sevegnani](http://www.dcs.gla.ac.uk/~michele), [Blair Archibald](http://www.blairarchibald.co.uk)

----------------------------------------------------------------------------

BigraphER is a command-line tool to compute the transition system of a BRS and
export it to the stochastic model checker [PRISM](prism) or to graphical form.

To get started, checkout the
[Install](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#inst) and
[Usage](http://www.dcs.gla.ac.uk/~michele/bigrapher.html#tool) guides.

### Documentation

#### User manual

The main documentation entry point to BigraphER is the man page. It can be
viewed with command `man bigrapher`.

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
