(** Types of rective systems.
    @author Michele Sevegnani *)

type t = BRS | PBRS | SBRS
         
val to_string : t -> string

val to_string_ext : t -> string

val sim_type : t -> string

val ts_type : t -> string
  
val limit_type : t -> string

val limit_msg : t -> string
