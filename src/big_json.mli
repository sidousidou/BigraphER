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

(** {3 Decoder} *)


(**/**)
