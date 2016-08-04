type bound = Big.bg * Big.bg
type rpo = Big.bg * Big.bg * Big.bg
                        
(** Four bigraphs Ai and Di as well as isomporphisms for each to denote the node to node relations **)
val rpo: bound -> bound -> int Iso.t -> int Iso.t -> int Iso.t -> int Iso.t -> rpo
