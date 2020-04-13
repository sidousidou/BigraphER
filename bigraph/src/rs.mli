(** Types of rective systems.

    @author Michele Sevegnani *)

(** Kinds of transition systems. *)
type t =
  | BRS  (** Bigraphical Reactive Systems *)
  | PBRS  (** Probabilistic Bigraphical Reactive Systems *)
  | SBRS  (** Stochasrtic Bigraphical Reactive Systems *)

(** Raised when an export error occurs. *)
exception EXPORT_ERROR of string

val to_string : t -> string

val to_string_ext : t -> string

val sim_type : t -> string

val ts_type : t -> string

val limit_type : t -> string

val limit_msg : t -> string

val module_id : t -> string
