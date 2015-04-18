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

(** This module provides set operations for nodes of bigraphs. Each element is
    in the form [(v,c)], where [v] is an unique node identifier and [c] is a
    control. *)
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

  (** [find_all ns c] returns the list of nodes of control [c] in node set
  [ns]. *)
  val find_all : t -> Ctrl.t -> int list 

  (** [tens n0 n1] returns the disjoint union of name sets [n0] and [n1]. *)
  val tens : t -> t -> t
    
  (** [abs ns] returns an ordered list of controls, i.e. node identifiers are
      dropped. *)
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

  val norm : t -> string
 
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

  (** {5 Standard set operations} *)	 
	 
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

  (** {5 Additional functions} *)
					
  (** [to_string s] gives the string representation of port set [s]. For
      example: [{(0, 0), (0, 1)}]. *)
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

(**/**)
