###0.6.0 (2015-04-11):###

*  Rate for stochastic reaction rules are now specified by ```-[ rate ]->```.
*  Instantiation maps for (stochastic) reaction rules are specified by ```@```.
   This feature is not implemented yet.
*  Typing discipline is stricter now (i.e. no implicit casting is performed).
*  Equal parameters do not vary in reaction instantiations. For example,
   ```R(t,t)``` is always instantiated with the same values for ```t```.
*  Introduced support for the declaration of lists of parameters. Example:
   ```int t, t' = {0, 5, 8, 24};```.
   