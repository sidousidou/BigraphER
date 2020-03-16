(lang dune 2.0)
(name bigrapher)
(version 1.9.2)
(generate_opam_files true)
(using menhir 1.0)

(source (uri https://bitbucket.org/mseve/bigrapher/))
(license BSD-2-Clause)
(authors "Michele Sevegnani" "Blair Archibald")
(maintainers "michele.sevegnani@glasgow.ac.uk")
(homepage "http://www.dcs.gla.ac.uk/~michele/bigrapher.html")
(documentation "http://www.dcs.gla.ac.uk/~michele/docs/_doc/bigrapher")

(package
  (name bigrapher)
  (synopsis "Implementation of Bigraphical Reactive Systems (BRS)")
  (description "BigraphER is an efficient implementation of Bigraphical Reactive Systems (BRS)
that supports bigraphs with sharing, stochastic and probabilistic reaction
rules, rule priorities, graphical output and predicate checking. Models can be
exported to probabilistic model checker PRISM.")
  (tags ("bigraphs" "rewriting engine" "stochastic simulation" "org:dcs"))
  (depends
    (dune (and :build (>= 2.0.0)))
    (menhir :build)
    (conf-graphviz (>= 2.38.0))
    (bigraph (>= 1.3.2))
    (big_json (>= 0.2.1))
    (cmdliner (>= 1.0.4))
    (odoc :with-doc)))
