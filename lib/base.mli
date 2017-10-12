(** This module provides some basic library functions.  
    @author Michele Sevegnani *)

(** {3 Specialised maps} *)

module M_int : Map.S with type key = int

module M_string : Map.S with type key = String.t

(** {3 Specialised sets} *)

module S_string : Set.S with type elt = String.t
     
(** {3 Specialised hash tables} *)

module H_int : Hashtbl.S with type key = int

module H_string : Hashtbl.S with type key = string


(** {3 JSON format} *)
module JSON :
sig
  type json_node =
    | J_int of string * int
    | J_string of string * string
    | J_array of string * json_node list
    | J_record of string * json_node list
    | J_node of json_node list
  val to_string : json_node -> string
end

(** {3 Helper functions} *)

(** [safe (Some v)] returns value [v]. Raises an exception on [None].

    @raise Assert_failure when argument is [None]. *)  
val safe : 'a option -> 'a

(** [safe_exn f] tries to execute [f].

    @raise Assert_failure when [f] throws an exception. *)  
val safe_exn : 'a -> 'a

(** Integer equality. *)
val int_equal : int -> int -> bool

(** Compare integers. *)			  
val int_compare : int -> int -> int

(** Compare pairs of integers. *)			  
val ints_compare : int * int -> int * int -> int

(** Flip order of the arguments *)					       
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** Flip order of the last two arguments *)					       
val flip2 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'c -> 'b -> 'd

(** Split string on new lines. *)
val parse_lines : string -> string list

(** Remove the first and last characters of a string.

    @raise Invalid_argument if the input string's lenght is less than [2]. *)
val remove_block_delims : string -> string
(**/**)
