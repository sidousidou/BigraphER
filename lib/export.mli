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
							     
(* (\** [write_big b name path ] writes an svg representation of bigraph [b] on file *)
(*     [name] in directory [path] and returns the number of bytes written. *\) *)
(* val write_big : Big.bg -> string -> string -> int *)
					
(* (\** Same as {!Export.write_big} but for transition systems. *\) *)
(* val write_ts : Brs.TS.t -> string -> string -> int *)

(* (\** Same as {!Export.write_big} but for Continuous Time Markov Chains. *\) *)
(* val write_ctmc : Sbrs.ctmc -> string -> string -> int *)

(* (\** Export a transition system to a text file in PRISM format. *\) *)
(* val write_ts_prism : Brs.TS.t -> string -> string -> int *)

(* (\** Same as {!Export.write_ts_prism} but for Continuous Time Markov Chains. *\) *)
(* val write_ctmc_prism : Sbrs.ctmc -> string -> string -> int *)

(* (\** Export a labelling function to a text file in PRISM format. *\) *)
(* val write_csl : (int, int) Hashtbl.t -> string -> string -> int *)
