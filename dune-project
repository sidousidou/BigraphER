(lang dune 2.5)
(version 1.9.2)
(name bigraph-tools)
(source (uri https://bitbucket.org/uog-bigraph/bigraph-tools/))
(license BSD-3-Clause)
(authors "Michele Sevegnani" "Blair Archibald")
(maintainers "michele.sevegnani@glasgow.ac.uk")
(homepage "http://www.dcs.gla.ac.uk/~michele/bigrapher.html")
(generate_opam_files true)

(using menhir 1.0)

(package
  (name minisat)
  (version 0.5.3)
  (synopsis "Bindings for the SAT solver MiniSAT")
  (tags ("SAT solver" "org:dcs"))
  (depends
    (dune (and :build (>= 2.0.0)))
    (dune-configurator :build)
    (odoc :with-doc)))

(package
  (name bigraph)
  (version 1.3.2)
  (synopsis "Library to programmatically manipulate bigraphs and Bigraphical Reactive Systems")
  (description "bigraph is a library to programmaticaly manipulate bigraphs, reaction rules and Bigraphical Reactive Systems (BRS). It supports both bigraphs and bigraphs with sharing, probabilistic and stochastic reaction rules, rule priorities, rules with instantiation maps, parameterised controls, simulation, exhaustive state space exploration, export to probabilistic model checker PRISM, and predicate checking.")
  (tags ("bigraphs" "rewriting" "org:dcs"))
  (depends
    (dune (and :build (>= 2.0.0)))
    (ocaml (> 4.05))
    (minisat (>= 0.5.3))
    (odoc :with-doc))
  (conflicts
    (bigrapher (<= 1.6.1))))

(package
  (name big_json)
(version 0.2.1)
  (synopsis "JSON codec for bigraphical structures")
  (description "big_json is a codec to encode and decode bigraphical structures such as bigraphs
and reaction rules to the JSON data format. The library provides a JSON
interface to the matching engine of the bigraph library. This is also exposed by
the big_match command line tool.")
  (tags ("bigraphs" "json" "codec" "org:dcs"))
  (depends
    (dune (and :build (>= 2.0.0)))
    (jsonm (>= 1.0.1))
    (bigraph (>= 1.3.1))
    (odoc :with-doc)))

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
    (big_json (>= 0.2.1))
    (cmdliner (>= 1.0.4))
    (odoc :with-doc)))
