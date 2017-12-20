(** JSON codec for bigraphical structures
    @author Michele Sevegnani *)

(** {3 Encoder} *)

val ctrl_to_json : ?minify:bool -> Bigraph.Ctrl.t -> String.t 

val nodes_to_json : ?minify:bool -> Bigraph.Nodes.t -> String.t 

val place_to_json : ?minify:bool -> Bigraph.Place.t -> String.t 

val link_to_json : ?minify:bool -> Bigraph.Link.Lg.t -> String.t 

val big_to_json : ?minify:bool -> Bigraph.Big.t -> String.t 

val react_to_json : ?minify:bool -> Bigraph.Brs.react -> String.t 

val preact_to_json : ?minify:bool -> Bigraph.Pbrs.react -> String.t 

val sreact_to_json : ?minify:bool -> Bigraph.Sbrs.react -> String.t 

val occs_to_json : ?minify:bool -> Bigraph.Brs.occ list -> String.t 

val p_occs_to_json : ?minify:bool -> Bigraph.Pbrs.occ list -> String.t 

val s_occs_to_json : ?minify:bool -> Bigraph.Sbrs.occ list -> String.t 

val matches_to_json : ?minify:bool -> Bigraph.Big.occ list -> String.t 

(** {3 Decoder} *)


(**/**)
