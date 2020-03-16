(** This module provides a function to find a RPO in
    a bound of epimorphic place graphs.
    This module has been written within an EPSRC funded
    research summer internship.

    @author W. David Frohlingsdorf*)

type bound = Big.bg * Big.bg 
type rpo = bound * Big.bg
type rpo_match = int Iso.t * int Iso.t * int Iso.t * int Iso.t

(** Four bigraphs Ai and Di as well as isomporphisms for each to denote the node to node relations **)
val rpo: bound -> bound -> rpo_match -> rpo
