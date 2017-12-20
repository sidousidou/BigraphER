(** JSON codec for bigraphical structures *)
open Bigraph
    
(* Encoder *)

val ctrl_to_json : ?minify:bool -> Ctrl.t -> String.t 

val nodes_to_json : ?minify:bool -> Nodes.t -> String.t 

val place_to_json : ?minify:bool -> Place.t -> String.t 

val link_to_json : ?minify:bool -> Link.Lg.t -> String.t 

val big_to_json : ?minify:bool -> Big.t -> String.t 

(* Decoder*)
