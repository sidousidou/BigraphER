###1.3.3 (2020-09-07): ###

* Performance improvements
* Support pseudoboolean (MiniCARD) solver backend
* Add support for application conditions
* `Big.decomp` now returns a minimal parameter decomposition

###1.3.2 (2020-02-25): ###

* Fix incorrect identity element for iterated operators
* Fix PRISM label output to sanitise parametered predicates and add missing semicolon when required

###1.3.1 (2019-11-11): ###

* Migrate build system to `dune 2.0.0`

###1.3.0 (2018-07-16): ###

* Add support for state-labels as comma-separated list of matching predicates
  and edge-labels as rules names in Graphviz output
* Optimise `raise` to `raise_notrace`
* Add custom toplevel `bigtop.ml`
* Migrate build system to `dune 1.0.0`
* Fix pretty printers
* Fix computation of number of matches
* Fix bug occurring when duplicate states are returned by scanning through
  priorities


###1.2.0 (2018-04-24): ###

* Use `newrank` to fix dot generation
* Add some pretty printers
* Fix bug in matching engine


###1.0.1 (2018-03-13): ###

* Breaking change in `Fun.check_codom` 


###1.0.1 (2018-03-11): ###

* Fix bug occurring when a reaction rule's instantiation map is a non-surjective
  identity


###1.0.0 (2017-12-19): ###

* Split library from bigrapher package
