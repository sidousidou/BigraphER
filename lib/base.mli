(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on basic bigraphical entities such as 
    controls, nodes and ports.
    @author Michele Sevegnani *)

val safe : 'a option -> 'a

(** {6 Controls} *)

(** The module of bigraphical controls. *)
module Ctrl:
sig
  type t = Ctrl of string * int
  (** [Ctrl (s, ar)] creates a control of arity [ar] from string [s]. *)

  (** [to_string c] gives the string representation of control [c].*)
  val to_string : t -> string

  (** [arity c] returns the arity of control [c].*)
  val arity : t -> int

  (** [name c] gives the string defining a control, ignoring parameters. For instance, 
      [name (Ctrl "A(3,7)")] returns ["A"]. *)
  val name : t -> string

  (** Equality for type {!Base.Ctrl.t}. *)
  val (=) : t -> t -> bool
    
  (** Comparison function *)
  val compare : t -> t -> int

  (** Parses the control string to obtain a list of actuals:
    [Control(a0,a1,a2) --> a0;a1;a2] *)
  val acts : t -> string list
end

(** {6 Nodes} *)

(** This module provides set operations for nodes of bigraphs. Each element is 
    in the form [(v,c)], where [v] is an unique node identifier and [c] is a 
    control.*)
module Nodes :
sig
  type t = { ctrl : (int, Ctrl.t) Hashtbl.t;
	     sort : (string, int) Hashtbl.t;
	     size : int; 
	   }
  val add : t -> int -> Ctrl.t -> t
  val fold : (int -> Ctrl.t -> 'a -> 'a) -> t -> 'a -> 'a

  val empty : unit -> t
  
  val is_empty : t -> bool
  
  (** [find ns i] gives the control of node [i] in node set [ns]. *)
  val find : t -> int -> Ctrl.t 

  (** [find_all ns c] returns the list of nodes of control [c] in node set [ns]. *)
  val find_all : t -> Ctrl.t -> int list 

  (** [tens n0 n1] returns the disjoint union of name sets [n0] and [n1]. *)
  val tens : t -> t -> t
    
  (** [abs ns] returns an ordered list of controls, i.e. node identifiers
      are dropped. *)
  val abs : t -> Ctrl.t list
    
  val to_string : t -> string

  val parse : string -> (int, int) Hashtbl.t -> t
   
  (** [to_dot ns] returns a string expressing node shapes in dot format. *)
  val to_dot: t -> string
    
  (** Apply an isomorphism *)
  val apply_exn : t -> int Iso.t -> t

  (** Apply an isomorphism only to nodes in the domain of the isomorphism. 
      Other nodes are discarded. *)
  val filter_apply_iso : t -> int Iso.t -> t
  
  val not_sub : t -> t -> bool

  val norm: t -> string
 
  val equal : t -> t -> bool

end

(** {6 Ports} *)

(** This module provides set operations for ports of nodes. Each element is in 
    the form [(v,w)], where [v] is an unique node identifier and [w] is a port 
    identifier which is unique in an edge. *)
module Ports :
sig
	type elt = int * int
	type t
	val empty : t
	val is_empty : t -> bool
	val mem : elt -> t -> bool
	val add : elt -> t -> t
	val singleton : elt -> t
	val remove : elt -> t -> t
	val union : t -> t -> t
	val inter : t -> t -> t
	val diff : t -> t -> t
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val subset : t -> t -> bool
	val iter : (elt -> unit) -> t -> unit
	val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
	val for_all : (elt -> bool) -> t -> bool
	val exists : (elt -> bool) -> t -> bool
	val filter : (elt -> bool) -> t -> t
	val partition : (elt -> bool) -> t -> t * t
	val cardinal : t -> int
	val elements : t -> elt list
	val min_elt : t -> elt
	val max_elt : t -> elt
	val choose : t -> elt
	val split : elt -> t -> t * bool * t

	(** [to_string ps] gives the string representation of port set [ps].*)
	val to_string : t -> string

	(** [of_nodes ns] transform a set of nodes into a set of ports. *)
	val of_nodes : Nodes.t -> t

	(** Construct a list of control strings. *)
	val types : t -> Nodes.t -> string list

	(** [to_IntSet ps] returns a set of node identifiers form a set of ports.*)
	val to_IntSet : t -> IntSet.t

	(** Apply an isomorphism *)
	val apply_exn : t -> int Iso.t -> t
	
	(* (\** [sub_multiset a b] returns [true] if [a] is a submultiset of [b].  *)
	(*     Port identifiers are ignored. *\)   *)
	(* val sub_multiset : t -> t -> bool   *)

	(** Construct a mapping from nodes to number of port occurrences
	    within a port set. *)  
	val arities : t -> (int * int) list

	(** Construct a list of possible node assignments starting from two
	    compatible port sets. *)
	val compat_list : t -> t -> Nodes.t -> Nodes.t -> Cnf.lit list list  
end
