###1.1.0 (2017-03-10): ###

* Improve error messages in reaction rules interface checks
* Use new CSS for the documentation
* Add `merge` and `split` keywords
* Add `--export-ml` flag to export models to OCaml
* Add support for `txt` output format
* Extend parser to support renamings. Example: `x/{x0,x1} b`
* Disallow duplicate names when specifying interfaces
* FIX: capture `NOT_PRIME` exception at run-time
* FIX: typos in the documentation
* FIX: warning 44

###1.0.2 (2016-05-20):###

* Use maps to implement ports.
* Compile with `strict-formats`.
* Add `--no-colors` flag to disable ANSI colouring in the output.
* FIX: check codomain of empty instantiation maps.
* FIX: classes with higher priority are checked again after a reducible class is
  reduced to the fixpoint.


###1.0.1 (2016-02-08):###

* Use maps instead of hash tables.
* More informative messages in reaction rule validity checks.


###1.0.0 (2016-01-27):###

* Add predicates checking in the BRS loop.
* Dot files can be exported even if dot is not installed.
* FIX: error in tensor product.
* FIX: graphical output of links.
* FIX: update progress bar during simulations.


###0.7.1 (2015-07-05):###

* Allow ``'`` in control identifiers.
* Implement `validate` switch.
* Add ground check for `init`.
* FIX: graphical notation for links.
* FIX: instantiation maps.
* FIX: `dot` default format if `graphviz` is not available.


###0.7.0 (2015-05-13):###

* Complete API documentation.
* Improve performance (~3%) with hashing on bigraph keys.
* New cli interface with subcommands `full`, `sim`, and `validate`.
* New man pages for subcommands.
* Support for raw `dot` output.


###0.6.2 (2015-05-04):###

* FIX: occurrence counting in simulations.
* Sort `tra` PRISM output by node index.


###0.6.1 (2015-04-24):###

* Install API reference in OPAM.
* FIX: add `menhir` dependency in OPAM.
* Compile binaries with `-O3`.


###0.6.0 (2015-04-11):###

*  Rate for stochastic reaction rules are now specified by ```-[ rate ]->```.
*  Instantiation maps for (stochastic) reaction rules are specified by ```@```.
   This feature is not implemented yet.
*  Typing discipline is stricter now (i.e. no implicit casting is performed).
*  Equal parameters do not vary in reaction instantiations. For example,
   ```R(t,t)``` is always instantiated with the same values for ```t```.
*  Introduced support for the declaration of lists of parameters. Example:
   ```int t, t' = {0, 5, 8, 24};```.
   
