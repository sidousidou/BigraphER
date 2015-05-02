(** Module providing export utilities.
    @author Michele Sevegnani *)

(** The type of error *)
type error =
  | Dot_not_found
  | Dot_stopped of int
  | Dot_killed of int
  | Internal_error of Unix.error * string * string
  | Sys of string

(** Raised by [write_*] functions when an error is encountered. *)
exception ERROR of error

(** Return a string describing the error.*)
val report_error : error -> string
			      
val write_svg : string -> name:string -> path:string -> int

val write_string : string -> name:string -> path:string -> int
