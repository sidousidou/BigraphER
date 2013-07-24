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
    @author Michele Sevegnani
    @version 0.5 *)

(** {6 Controls} *)

(** The type of bigraphical controls. *)
type ctrl = Ctrl of string * int
(** [Ctrl (s, ar)] creates a control of arity [ar] from string [s]. *)

(* [sort ctrls] generates a sort for controls [ctrls]. The sort is encoded as
    a control whose name is the concatenation of the names of the controls in
    list [ctrls]. For example [sort [Ctrl "A"; Ctrl "B"; Ctrl "C"]] produces 
    [Ctrl "A|B|C"].
val sort : ctrl list -> ctrl *)

(** [string_of_ctrl c] gives the string representation of control [c].*)
val string_of_ctrl : ctrl -> string

(** [arity c] returns the arity of control [c].*)
val arity : ctrl -> int

(** Equality for tyep [ctrl]. *)
val ctrl_equals : ctrl -> ctrl -> bool

(** Parses the control string to obtain a list of actuals:
    [Control(a0,a1,a2) --> a0;a1;a2] *)
val acts_of_ctrl : ctrl -> string list

(** [name_of_ctrl c] gives the string defining a control, for instance 
    [name_of_ctrl (Ctrl "A(3,7)")] returns ["A"] .*)
val name_of_ctrl : ctrl -> string

(** {6 Nodes} *)

(** This module provides set operations for nodes of bigraphs. Each element is 
    in the form [(v,c)], where [v] is an unique node identifier and [c] is a 
    control.*)
module Nodes :
sig
	type elt = int * ctrl
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
end

(** [ctrl_of_node i ns] gives the control of node [i] in node set [ns].*)
val ctrl_of_node : int -> Nodes.t -> ctrl

(** [uplus n0 n1] returns the disjoint union of name sets [n0] and [n1].*)
val uplus : Nodes.t -> Nodes.t -> Nodes.t

(** [string_of_nodes ns] gives the string representation of node set [ns].*)
val string_of_nodes : Nodes.t -> string

(** [abs_nodes ns] returns an ordered list of controls, i.e. node identifiers
    are dropped. *)
val abs_nodes : Nodes.t -> ctrl list

(** [get_dot ns] returns a string expressing node shapes in dot format. *)
val get_dot : Nodes.t -> string

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
end

(** [string_of_ports ps] gives the string representation of port set [ps].*)
val string_of_ports : Ports.t -> string

(** [ports_of_nodes ns] transform a set of nodes into a set of ports. *)
val ports_of_nodes : Nodes.t -> Ports.t

(** Construct a list of the cardinalities of the ports belonging to
   the same node. Example: [(1,0);(1,1);(2,0)] -> [1;2] *)
val card_ports : Ports.t -> int list

(** Construct a list of control strings.*)
val type_of_ports : Ports.t -> Nodes.t -> string list

(*
(** Construct a list of lists of pairs representing a set of clauses. The first
    element in every list has to be negated.*)
val clauses_of_ports : int -> int -> Ports.t -> Ports.t -> (int * int) list list*)
(** {6 Sets of integers} *)

(** This module provides operations for sets of int.*)
module Int_set :
sig
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
end

(** [string_of_Int_set s] gives the string representation of [Int_set s].*)
val string_of_Int_set : Int_set.t -> string

(** [set_of_list l] returns a set of int form a list *)
val set_of_list : int list -> Int_set.t

(** [set_of_ports ps] returns a set of node identifiers form a set of ports.*)
val set_of_ports : Ports.t -> Int_set.t

(** [of_int i] returns a set [{0, 1, ...., i-1}].*)
val of_int: int -> Int_set.t

(** [off i s] adds offset [i] to all the elements of set [s].*)
val off: int -> Int_set.t -> Int_set.t

(** [norm s] nomalises set [s]: e.g. [{4, 6, 7, 9} --> {0, 1, 2, 3}] *)
val norm : Int_set.t -> Int_set.t

(** {6 Binary relations} *)

(** This module provides set operations on binary relations. Elements are pairs
    of integers. *)
module Iso :
sig
	type elt = int * int (** Usually, the first element is a node in the pattern
		bigraph, while the second is an element of the target bigraph. *)
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
end

val string_of_iso : Iso.t -> string

(** [get_i e i] returns the image via isomorphism [i] of element [e], i.e. 
    [i(e)].
    @raise Not_found if element [e] is not in the domain of isomorphism [i]. *)
val get_i : int -> Iso.t -> int

(** [get_inv_i e i] returns the inverse via isomorphism [i] of element [e], 
    i.e. [i^-1(e)].
    @raise Not_found if element [e] is not in the codomain of isomorphism [i].*)
val get_inv_i : int -> Iso.t -> int

(** [inverse i] returns [i^-1]. *)
val inverse : Iso.t -> Iso.t

(** [of_list l] returns an isomorphism with the elements in list [l].*)
val of_list : (int * int) list -> Iso.t

(** [hash_of_iso i] returns a hash table representing relation [i]. *)
val hash_of_iso : Iso.t -> (int, int) Hashtbl.t

(** [apply s i] returns the image via isomorphism [i] of all the elements in 
    set [s].
    @raise Not_found if some element in [s] is not in the domain of isomorphism
    [i]. *)
val apply : Int_set.t -> Iso.t -> Int_set.t

(** [dom i] returns the domain of isomorphism [i].*)
val dom : Iso.t -> Int_set.t

(** [codom i] returns the codomain of isomorphism [i].*)
val codom : Iso.t -> Int_set.t

(** [union_list l] computes the union of the isos in list [l]. *)
val union_list : Iso.t list -> Iso.t

(** [fix_num s] generates an isomorphism to fix the numbering of [s]: e.g. 
    [{2, 5, 6, 7} --> {(2,0), (5,1), (6,2), (7,3)}]*)
val fix_num : Int_set.t -> Iso.t

(** [apply_nodes n i] applies isomorphism [i] to set of nodes [n]. [i] is 
    assumed to be total over [n].*)
val apply_nodes : Nodes.t -> Iso.t -> Nodes.t

(** [apply_ports ps i] applies isomorphism [i] to set of ports [ps].*)
val apply_ports : Ports.t -> Iso.t -> Ports.t

(** [match_nodes t p] returns an iso from nodes in [p] to nodes in [t] having
     different controls.*)
val match_nodes: Nodes.t -> Nodes.t -> Iso.t

(** [multiset_of_ports p] returns an isomrphism with domain the cardinality of
    the port set of a node and with codomain the node indexes. *)
val multiset_of_ports : Ports.t -> Iso.t

(** [set_cart a b] returns the cartesian product of sets [a] and [b] *)
val set_cart : Int_set.t -> Int_set.t -> Iso.t

(** [is_id i] returns [true] if iso [i] is an identity, [false] otherwise.*)
val is_id : Iso.t -> bool

(** Generate equivalent isomorphisms. *)
val gen_isos : Iso.t * Iso.t -> (Iso.t * Iso.t) list -> (Iso.t * Iso.t) list

(** {6 Combinatorics} *)

(** [int_interval start delta end] computes a list of integers with first 
    element [start], last element [end], and interval between the elements 
    [delta].*)
val int_interval : int -> int -> int -> int list

(** [float_interval start delta end] computes a list of [float] with first 
    element [start], last element [end], and interval between the elements 
    [delta].*)
val float_interval : float -> float -> float -> float list

(** [par_comb l] computes all the possible combinations of the elements of [l].
    For example, [par_comb \[\[1;2\];\[3\];\[5;6;1\]\]] gives [\[\[1; 3; 5\]; 
    \[1; 3; 6\]; \[1; 3; 1\]; \[2; 3; 5\]; \[2; 3; 6\]; \[2; 3; 1\]\]].*)
val par_comb : 'a list list -> 'a list list

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

val cart_of_list_iso : Iso.t list -> Iso.t list

(**/**)
