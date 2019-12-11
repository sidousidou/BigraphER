(lang dune 2.0)
(name bigraph)
(version 1.3.0)
(generate_opam_files true)

(source (uri https://bitbucket.org/mseve/bigraph/))
(license BSD-2-Clause)
(authors "Michele Sevegnani" "Blair Archibald")
(maintainers "michele.sevegnani@glasgow.ac.uk")
(homepage "http://www.dcs.gla.ac.uk/~michele/bigrapher.html")
(documentation "http://www.dcs.gla.ac.uk/~michele/docs/_doc/bigraph")

(package
  (name bigraph)
  (synopsis "Library to programmatically manipulate bigraphs and Bigraphical Reactive Systems")
  (description "bigraph is a library to programmaticaly manipulate bigraphs, reaction rules and Bigraphical Reactive Systems (BRS). It supports both bigraphs and bigraphs with sharing, probabilistic and stochastic reaction rules, rule priorities, rules with instantiation maps, parameterised controls, simulation, exhaustive state space exploration, export to probabilistic model checker PRISM, and predicate checking.")
  (tags ("bigraphs" "rewriting" "org:dcs"))
  (depends
    (dune (and :build (>= 2.0.0)))
    (minisat (>= 0.5.3))
    (odoc :with-doc))
  (conflicts
    (bigrapher (<= 1.6.1))))  

