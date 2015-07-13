###0.7.1 (2015-07-05):###

* Allow ``'`` in control identifiers.
* Implement `validate` switch.
* Add ground check for `init`.
* FIX: graphical notation for links.
* FIX: instantiation maps.
* FIX: dot default format if graphviz is not available.


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
* FIX: add menhir dependency in OPAM.
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
   
