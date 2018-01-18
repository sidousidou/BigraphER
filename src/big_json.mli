(** JSON codec for bigraphical structures
    @author Michele Sevegnani *)

(** {3 Encoder} *)

val big_to_json : ?minify:bool -> Bigraph.Big.t -> String.t 

val react_to_json : ?minify:bool -> Bigraph.Brs.react -> String.t 

val preact_to_json : ?minify:bool -> Bigraph.Pbrs.react -> String.t 

val sreact_to_json : ?minify:bool -> Bigraph.Sbrs.react -> String.t 

val occs_to_json : ?minify:bool -> Bigraph.Brs.occ list -> String.t 

val p_occs_to_json : ?minify:bool -> Bigraph.Pbrs.occ list -> String.t 

val s_occs_to_json : ?minify:bool -> Bigraph.Sbrs.occ list -> String.t 

val matches_to_json : ?minify:bool -> Bigraph.Big.occ list -> String.t 

(** {3 Decoder} *)

val big_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Big.t, String.t) result

val react_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Brs.react, String.t) result

val preact_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Pbrs.react, String.t) result

val sreact_of_json : ?encoding:Jsonm.encoding -> String.t  -> (Bigraph.Sbrs.react, String.t) result

(** {3 Interface to the matching engine} *)

(** Compute the set of reachable states in one step. Note that isomorphic states
    are merged. *)
val step : ?encoding:Jsonm.encoding -> ?minify:bool -> String.t -> String.t

(**/**)
