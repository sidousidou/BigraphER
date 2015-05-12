(** This module provides operations on basic bigraphical entities such as
    controls, nodes and ports.  
    @author Michele Sevegnani *)

(** {6 Controls} *)

(** The module of bigraphical controls. *)
module Ctrl :
sig
  
  (** [Ctrl (s, ar)] creates a control of arity [ar] named [s]. *)
  type t = Ctrl of string * int

  (** [to_string c] gives the string representation of control [c] in the form
      [name:arity]. *)
  val to_string : t -> string

  (** [arity c] returns the arity of control [c]. *)
  val arity : t -> int

  (** Equality for type {!Base.Ctrl.t}. *)
  val (=) : t -> t -> bool
			
  (** Comparison function. *)
  val compare : t -> t -> int
			    
end

(** {6 Nodes} *)

(** This module provides operations on bigraphical nodes. *)
module Nodes :
sig

  (** The type of a set of nodes. *)		
  type t =
    {
      ctrl : (int, Ctrl.t) Hashtbl.t; (** Map between node identifiers and controls. *)
      sort : (string, int) Hashtbl.t; (** Map between controls and node identifiers. *)
      size : int                      (** Cardinality of the set. *)
    }

  (** Add a node to the set. *)
  val add : t -> int -> Ctrl.t -> t

  (** Fold over a set. *)				    
  val fold : (int -> Ctrl.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** The empty node set. *)
  val empty : unit -> t

  (** Return [true] if the set is empty. *)			
  val is_empty : t -> bool

  (** Return a string representation of a node set. Example: ["\{(2, Ready:0),(0,
      A:1),(3, Fun:0),(1, Snd:2)\}"]. *)
  val to_string : t -> string

  (** [to_dot ns] returns a string expressing node shapes in dot format. *)
  val to_dot: t -> string
			 			
  (** [get_ctrl_exn ns i] returns the control of node [i] in node set [ns].  
      @raise Not_found if the node identifier is not present in the set. *)
  val get_ctrl_exn : t -> int -> Ctrl.t 

  (** [find_all ns c] returns the list of nodes of control [c] in node set
      [ns]. *)
  val find_all : t -> Ctrl.t -> int list 

  (** [tens n0 n1] returns the disjoint union of name sets [n0] and [n1]. *)
  val tens : t -> t -> t

  (** [parse s h] parses the string representation of a node set. Argument [h]
      is a map between node identifiers and arities. An example for [s] is: ["A
      A B D"]. *)
  val parse : string -> (int, int) Hashtbl.t -> t
   
  (** Apply an isomorphism.
      @raise Not_found if a node identifier is not in the domain of the
      isomorphism.  *)
  val apply_exn : t -> int Iso.t -> t

  (** Apply an isomorphism only to nodes in the domain of the isomorphism.
      Other nodes are discarded. *)
  val filter_apply_iso : t -> int Iso.t -> t

  (** [not_sub a b] returns [true] when node set [a] is not a subset of node set
      [b]. *)
  val not_sub : t -> t -> bool

  (** Compute the norm of a set. The norm is a defined as a sorted list of
      controls. Example: [["A"; "A"; "C"; "D"; "T"]]. *)
  val norm : t -> string list

  (** Equality test. Node identities are ignored. *)		    
  val equal : t -> t -> bool

end

(** {6 Ports} *)

(** This module provides set operations for ports of nodes. *)
module PortSet :
sig

  (** A port is a pair [(v, w)], where [v] is an unique node identifier and [w]
      is a port identifier which is unique within an edge. *)
  type port = int * int

  (** The type of sets of ports *)		      
  type t

  (** {6 Standard set operations} *)	 
  (** These functions are described in the {{:
      http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.Make.html } standard
      library}. *)

  val empty : t
  val is_empty : t -> bool
  val mem : port -> t -> bool
  val add : port -> t -> t
  val singleton : port -> t
  val remove : port -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (port -> unit) -> t -> unit
  val fold : (port -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (port -> bool) -> t -> bool
  val exists : (port -> bool) -> t -> bool
  val filter : (port -> bool) -> t -> t
  val partition : (port -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> port list
  val min_elt : t -> port
  val max_elt : t -> port
  val choose : t -> port
  val split : port -> t -> t * bool * t

  (** {6 Additional functions} *)
					
  (** [to_string s] gives the string representation of port set [s]. For
      example: [\{(0, 0), (0, 1)\}]. *)
  val to_string : t -> string

  (** [of_nodes ns] transform a set of nodes into a set of ports. *)
  val of_nodes : Nodes.t -> t

  (** Construct a list of control strings. *)
  val types : t -> Nodes.t -> string list

  (** [to_IntSet ps] returns a set of node identifiers form a set of ports. *)
  val to_IntSet : t -> IntSet.t

  (** Apply an isomorphism.  
      @raise Not_found if a node identifier is not in the domain of the
      isomorphism. *)
  val apply_exn : t -> int Iso.t -> t
				      
  (** Construct a mapping from nodes to number of port occurrences within a port
      set. *)
  val arities : t -> int Fun.t

  (** Construct a list of possible node assignments starting from two compatible
      port sets. *)
  val compat_list : t -> t -> Nodes.t -> Nodes.t -> Cnf.lit list list
							    
end

(** {6 Helper functions} *)

(** [safe (Some v)] returns value [v]. Raises an exception otherwise.
    @raise Assert_failure  when argument is [None]. *)  
val safe : 'a option -> 'a

(** Compare pairs of integers. *)			  
val ints_compare : int * int -> int * int -> int
			  
(**/**)
