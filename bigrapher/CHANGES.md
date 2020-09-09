###1.9.3-quickfix1 (2020-09-09) ###

* Fix missing updates from merge

###1.9.3 (2020-09-07): ###

* Allow RNG seeds to be specified
* Support pseudo-boolean solver selection
* Support for application conditions on reaction rules
* Support "Action" BRS

###1.9.2 (2020-02-25): ###

* Add support for iterated operators par(n, bexp) and ppar(n, bexp) that repeat
  a bigraph n times with | or || as requested.
* Allow Non-BRS models with empty rule sets
* Fix handling of negative number parsing

###1.9.1 (2019-12-13): ###

* Migrate builds to dune 2.0.0
* Add support for string expressions in declarations, control parameters, and
  BRS parameters
* Disabled support for ml export due to missing/incorrect results being produced

###1.9.0 (2019-11-25): ###

* Migrate builds to dune
* Use `Cmdliner` package to handle commandline and environment variable handling

###1.8.0 (2018-07-23): ###

* Support new features in `bigraph 1.3.0`: edge and state labels in graphical
  output


###1.7.0 (2018-01-26): ###

* Depend on external library `bigraph`


###1.6.1 (2017-11-23): ###

* Add `parse_react_unsafe` to modules `Brs`, `Pbrs`, and `Sbrs`


###1.6.0 (2017-11-21): ###

* Improve documentation for predicates, e.g. `Big.is_epi`, `Link.is_ground`, ..
* FIX: install big-mode in `emacs/site-lisp/`
* Export `Stats` module
* Simplify signature of modules `Brs`, `Pbrs`, and `Sbrs`
* Optimise `Big.rewrite` when instantiation map is the identity function
* FIX: ridefinition of `Brs.apply`
* Add `Big.size`


###1.5.1 (2017-11-06): ###

* FIX: Suppress coloured errors and warnings when `-n` flag is present


###1.5.0 (2017-10-27): ###

* Return option type in `Brs.apply`, `Pbrs.apply`, and `Sbrs.apply` 
* Use error-aware return types in the API
* FIX: call to `Unix.waitpid`
* FIX: leak of `Export.ERROR`
* FIX: `Node.of_string`, `Link.of_string`, and `Big.of_string`
* FIX: exception in `Nodes.of_string`
* Export `Iso` module
* Return a string in `Big.json_of_big` 


###1.4.0 (2017-09-22): ###

* Add support for `json` output format
* FIX: `Big.to_string` when link graph is empty


###1.3.0 (2017-09-09): ###

* Add labels in the APIs to disambiguate arguments with the same type
* Extend parser to support *merge* closures. Example: `/{x0,x1} b`
* FIX: links are broken after applications of rules with instantiation maps


###1.2.1 (2017-07-24): ###

* Use mandoc for man pages
* Add support to parse a model from stdin


###1.2.0 (2017-06-28): ###

* Add support for `false` predicates in `.lab` output
* Add support for Probabilistic BRS
* New syntax for BRS declarations: `begin [brs|pbrs|sbrs] ... end`
* Add `Ctrl + C` trap


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
   
