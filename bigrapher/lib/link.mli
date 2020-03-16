(** This module provides operations on link graphs.
    @author Hidden *)

(** The type of names. *)
type name = Nam of string

(** This module provides set operations for faces. *)
module Face :
  sig
    type elt = name
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
	
(** The type of edges. *)	
type edg = {
  i : Face.t; (** Inner face *)
  o : Face.t; (** Outer face *)
  p : Base.PortSet.t; (** Set of ports *)
}
  
(** This module provides set operations for link graphs. *)
module Lg :
  sig
    type elt = edg
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

(** [parse_face ns] computes a face starting from list of string names [ns]. *)
val parse_face : string list -> Face.t

(** [string_of_face f] computes the string representation of face [f]. *)
val string_of_face : Face.t -> string

(** [to_string l] computes the string representation of link graph [l]. *)
val to_string : Lg.t -> string

(** Parse a list of strings. A hash table associating nodes to arities
    is also returned. *)
val parse : string list -> (Lg.t * (int, int) Hashtbl.t)

(** [get_dot l] computes a four-elements tuple encoding the dot
    representation of link graph [l]. The first two elements represent
    inner and outer names shape declarations. The third element
    represent the hyperedges shape declarations.  The fourth element
    specifies the adjacency matrix. *)
val get_dot : Lg.t -> string * string * string * string 

(** [inner l] computes the inner face of link graph [l]. *)
val inner : Lg.t -> Face.t

(** [outer l] computes the outer face of link graph [l]. *)
val outer : Lg.t -> Face.t

(** [ports l] computes the set of ports of link graph [l]. *)
val ports : Lg.t -> Base.PortSet.t

(** [apply_exn i l] computes a link graph obtained by applying
    isomorphism [i] to [l].

    @raise Not_found when a node is not defined in the iso. *)
val apply_exn : int Iso.t -> Lg.t -> Lg.t

(** {6 Elementary link graphs} *)

(** [elementary_sub in out] computes a substitution consisting of a
    single edge in which [in] and [out] are the inner and outer face,
    respectively. *)
val elementary_sub : Face.t -> Face.t -> Lg.t

(** [elementary_ion f] computes an elementary ion with outer face
    [f]. *)
val elementary_ion : Face.t -> Lg.t

(** [elementary_id f] computes the identity over face [f], {e i.e.} one
    edge for each name in [f]. *)
val elementary_id: Face.t -> Lg.t

(** [id_empty] is the empty link graph. *)
val id_empty : Lg.t

(** {6 Operations} *)

(** Raised when the tensor product between two incompatible link
    graphs cannot be performed. The first element is the set of inner
    common names while the second is the set of outer common names. *)
exception NAMES_ALREADY_DEFINED of (Face.t * Face.t)

(** Raised when a composition between two incompatible link graphs
    cannot be performed. *)
exception FACES_MISMATCH of (Face.t * Face.t)
		 
(** [tens a b n] computes the tensor product of link graphs [a] and
    [b].  Argument [n] is the number of nodes of [a].

    @raise  NAMES_ALREADY_DEFINED when there are shared names. *)
val tens : Lg.t -> Lg.t -> int -> Lg.t

(** [ppar a b n] computes the parallel composition of link graphs [a]
    and [b].  [n] is the number of nodes of [a]. *)
val ppar : Lg.t -> Lg.t -> int-> Lg.t

(** [comp a b n] computes the composition of link graphs [a] and [b].
    Argument [n] is the number of nodes of [a].
    
    @raise FACES_MISMATCH when names in the mediating interfaces
    differ. *)
val comp : Lg.t -> Lg.t -> int -> Lg.t

(** {6 Predicates} *)
                                                                        
(** [is_id l] is [true] if link graph [l] is an identity, [false]
    otherwise. *)
val is_id : Lg.t -> bool

(** [is_mono l] is [true] if link graph [l] is monomorphic, [false]
    otherwise. *)
val is_mono : Lg.t -> bool

(** [is_epi l] is [true] if link graph [l] is epimorphic, [false]
    otherwise. *)
val is_epi : Lg.t -> bool

(** [is_ground l] is [true] if link graph [l] has no inner names, [false]
    otherwise. *)
val is_ground : Lg.t -> bool    
		      
(** [is_guard l] is [true] if no edges in link graph [l] have both
    inner and outer names, [false] otherwise. *)
val is_guard : Lg.t -> bool    

(** Compute the maximum number of ports in one edge of a link graph. *)			
val max_ports : Lg.t -> int

(** Compute a sorted list with the cardinalities of the port sets. *)			  
val cardinal_ports : Lg.t -> int list
			  
(** Compute the number of closed edges. *)			  
val closed_edges : Lg.t -> int

(** [closed_edges_iso l] computes the set of closed edges of link
    graph [l]. The computed isomorphism maps edge indices of the new link
    graph to indices of edges in [l]. *)
val closed_edges_iso : Lg.t -> Lg.t * int Iso.t

(** {6 Decompositions} *)

(** [decomp t p i_e i_c i_d f_e] computes the decomposition of target
    [t] given pattern [p], iso [i_e], and isos from nodes in [t] to
    nodes of [c] and [d], respectively. Argument [f_e] is a total
    function from links in the pattern to links in the target. Pattern
    [p] is assumed epi and mono and [i_e] is from edges in [p] to
    edges in [t]. Isos [i_c] and [i_d] are obtained by
    {!val:Place.decomp}. The results are link graph [c], [d] and [id]. *)
val decomp : Lg.t -> Lg.t -> int Iso.t -> int Iso.t -> 
  int Iso.t -> int Fun.t -> Lg.t * Lg.t * Lg.t

(** Compute the prime components of a link graph. See {!val:Place.decomp}. *)					    
val prime_components : Lg.t -> (int Iso.t) list -> Lg.t list 

(** {6 Matching constraints} *)

exception NOT_TOTAL

(** Compute constraints to match closed edges in the pattern to
    closed edges in the target. Controls are checked to exclude
    incompatible pairs. The output is a list of clauses a set of
    blocked columns and a set of blocking pairs.

   @raise NOT_TOTAL when no matches are found. *)
val match_edges : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  Cnf.clause list * IntSet.t * Cnf.clause list

(** Compute constraints to match isomorphic port sets in closed
    edges. *)
val match_ports : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  Cnf.clause list -> Cnf.clause list list

(** Compute constraints to match peers in the pattern with peers in
    the target. Auxiliary variables are introduced to handle matches
    with open edges.

    @raise NOT_TOTAL when no matches are found. *)
val match_peers : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  int * int * Cnf.clause list list * (int * int) list * 
  Cnf.clause list * int Iso.t * int Iso.t

(** Similar to {!Link.match_edges} but constraints are for
    equality. *)
val match_list_eq : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  Cnf.clause list * Cnf.clause list

(** Similar to {!Link.match_ports} but constraints are for
    equality. *)
val match_ports_eq : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  Cnf.clause list -> Cnf.clause list list

(**/**)
