(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on basic bigraphical entities such as 
    controls, nodes and ports. It also defines sets of int, binary relations
    and various functions for combinatorial problems.
    @author Michele Sevegnani *)

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

(** {6 Isomorphisms} *)

(** This module provides set operations on isomorphisms. Elements are pairs
    of integers. *)
module Iso :
sig
  
  type t = (int, int) Hashtbl.t

  val empty : unit -> t
  val mem : t -> int -> int -> bool
    
  (** [find iso i] returns the image via isomorphism [iso] of element [i], i.e. 
      [iso(i)].
      @raise Not_found if element [i] is not in the domain of isomorphism [iso]. *)
  val find : t -> int -> int

  val compare : t -> t -> int
  
  val equal : t -> t -> bool  

  val cardinal : t -> int
  
  val add : t -> int -> int -> unit

  val union : t -> t -> t

  val fold : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a    
    
  val iter : (int -> int -> unit) -> t -> unit
  
  (** Return the inverse of an isomorphism. *)
  val inverse : t -> t

  (** Compute the domain of an isomorphism. *)
  val dom : t -> int list
    
  (** Compute the co-domain of an isomorphism. *)
  val codom : t -> int list
    
  val to_string : t -> string
    
  (** [of_list l] returns an isomorphism with the elements in list [l]. *)
  val of_list : (int * int) list -> t

  (** Return the elements of an isomorphism. Order is unspecified. *)
  val to_list : t -> (int * int) list

  (** [is_id i] returns [true] if iso [i] is an identity, [false] otherwise.*)
  val is_id : t -> bool

  (** Generate the equivalent isomorphisms by using a list of automorphisms. *)
  val gen_isos : t -> t list -> t list

end

(** {6 Sets of integers} *)

(** This module provides operations for sets of int.*)
module IntSet: sig

  type elt = int
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
    
  (** [to_string s] gives the string representation of [Int_set s].*)
  val to_string : t -> string

  (** [of_list l] returns a set of int form a list *)
  val of_list : int list -> t

  (** [of_int i] returns a set [{0, 1, ...., i-1}].*)
  val of_int: int -> t

  (** [off i s] adds offset [i] to all the elements of set [s].*)
  val off: int -> t -> t

  (** [norm s] normalises set [s]: e.g. [{4, 6, 7, 9} --> {0, 1, 2, 3}] *)
  val norm : t -> t
    
  (** [fix s] generates an isomorphism to fix the numbering of [s]: e.g. 
      [{2, 5, 6, 7} --> {(2,0), (5,1), (6,2), (7,3)}]*)
  val fix : t -> Iso.t
  
  (** Apply an isomorphism *)
  val apply : t -> Iso.t -> t
  
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
  val find_all : t -> string -> int list 

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
  val apply_iso : t -> Iso.t -> t

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
	val apply : t -> Iso.t -> t
	
	(** [sub_multiset a b] returns [true] if [a] is a submultiset of [b]. 
	    Port identifiers are ignored. *)  
	val sub_multiset : t -> t -> bool  

	(** Construct an isomorphism from nodes to number of port occurrences
	    within a port set. *)  
	val arities : t -> Iso.t

	(** Construct a list of possible node assignments starting from two
	compatible port sets. *)
	val compat_list : t -> t -> Nodes.t -> Nodes.t -> Cnf.m_var list list  
end

(* 
(** [match_nodes t p] returns an iso from nodes in [p] to nodes in [t] having
     different controls.*)
val match_nodes: Nodes.t -> Nodes.t -> Iso.t

(** [multiset_of_ports p] returns an isomrphism with domain the cardinality of
    the port set of a node and with co-domain the node indexes. *)
val multiset_of_ports : Ports.t -> Iso.t

(** [set_cart a b] returns the Cartesian product of sets [a] and [b] *)
val set_cart : IntSet.t -> IntSet.t -> Iso.t
*)
(*
(** {6 Combinatorics} *)

(*
(** [int_interval start delta end] computes a list of integers with first 
    element [start], last element [end], and interval between the elements 
    [delta].*)
val int_interval : int -> int -> int -> int list

(** [float_interval start delta end] computes a list of [float] with first 
    element [start], last element [end], and interval between the elements 
    [delta].*)
val float_interval : float -> float -> float -> float list
*)
(*
(** [par_comb l] computes all the possible combinations of the elements of [l].
    For example, [par_comb \[\[1;2\];\[3\];\[5;6;1\]\]] gives [\[\[1; 3; 5\]; 
    \[1; 3; 6\]; \[1; 3; 1\]; \[2; 3; 5\]; \[2; 3; 6\]; \[2; 3; 1\]\]].*)
val par_comb : 'a list list -> 'a list list
*)

(** [sub_multi a b] returns [true] if multiset [b] is a sub-multiset of
    multiset [a], [false] otherwise. Multisets are represented as ordered list 
    with duplicates.*)
val sub_multi : 'a list -> 'a list -> bool 

(** [count l f] returns a list with the sum of rates of each element 
    in [l]. Function [f] is a comparison function. *)
val count : ('a * float) list -> ('a -> 'a -> bool) -> ('a * float) list 

(** [cart_of_list l] returns the cartesian product of the elements of [l]. For instance,
when input is [\[\[\[1\];\[2\]\]; \[\[3\];\[4\]\]\]] the result is 
    [\[\[1;3\];\[1;4\];\[2;3\];\[2;4\]\]].*)
val cart_of_list : 'a list list list -> 'a list list

(**/**)
*)
