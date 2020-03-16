(lang dune 2.0)
(name big_json)
(version 0.2.1)
(generate_opam_files true)

(source (uri https://bitbucket.org/mseve/big_json/))
(license BSD-2-Clause)
(authors "Michele Sevegnani" "Blair Archibald")
(maintainers "michele.sevegnani@glasgow.ac.uk")
(homepage "http://www.dcs.gla.ac.uk/~michele/bigrapher.html")
(documentation "http://www.dcs.gla.ac.uk/~michele/docs/_doc/big_json")

(package
  (name big_json)
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
