(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on link graphs.
    @author Michele Sevegnani *)

(** The type of names.*)
type name = Nam of string

(** This module provides set operations for faces.*)
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
	
(** The type of edges.*)	
type edg = {
  i: Face.t; (** Inner face *)
  o: Face.t; (** Outer face *)
  p: Base.Ports.t; (** Set of ports *)
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

(** Raised when a tensor product between two incompatible link graphs is
    attempted. The first element is the set of inner common names while the
    second is the set of outer common names. *)
exception NAMES_ALREADY_DEFINED of (Face.t * Face.t)

(** Raised when a composition between two incompatible link graphs is
    attempted. *)
exception FACES_MISMATCH of (Face.t * Face.t)

(** [parse_face ns] returns a face starting from list of string names [ns]. *)
val parse_face : string list -> Face.t

(** [string_of_face f] returns a string rapresentation of face [f]. *)
val string_of_face : Face.t -> string

(** [to_string l] returns a string representation of link graph [l]. *)
val to_string : Lg.t -> string

(** Parse a list of strings. An Hashtbl node -> arity is also returned. *) 
val parse : string list -> (Lg.t * (int, int) Hashtbl.t)

(** [get_dot l] returns four strings for the dot representation of link graph
    [l]. The first two elements represent inner and outer names shape
    declarations. The third element represent the hyperedges shape declarations.
    The fourth element specifies the adjacency matrix. *)
val get_dot : Lg.t -> string * string * string * string 

(** [inner l] returns the inner face of link graph [l]. *)
val inner : Lg.t -> Face.t

(** [outer l] returns the outer face of link graph [l]. *)
val outer : Lg.t -> Face.t

(** [ports l] returns the set of ports of link graph [l]. *)
val ports : Lg.t -> Base.Ports.t

(** [apply_iso i l] returns a link graph obtained by applying isomorphism [i]
    to [l].
    @raise Not_found when a node is not defined in the iso. *)
val apply_iso: Base.Iso.t -> Lg.t -> Lg.t

(** {6 Elementary link graphs} *)

(** [elementary_sub in out] returns a substitution consisting of a single
    edge where [in] and [out] are its inner and outer face, respectively.*)
val elementary_sub : Face.t -> Face.t -> Lg.t

(** [elementary_ion f] returns the link graph of an elementary ion with outer
    face [f]. *)
val elementary_ion : Face.t -> Lg.t

(** [elementary_id f] returns the identity over face [f], i.e. one edge for 
    each name in [f].*)
val elementary_id: Face.t -> Lg.t

(** [id_empty] is the empty link graph.*)
val id_empty : Lg.t

(** {6 Operations} *)

(** [tens a b n] returns the tensor product of link graphs [a] and [b].
    [n] is the number of nodes of [a].
    @raise  NAMES_ALREADY_DEFINED when there are shared names *)
val tens : Lg.t -> Lg.t -> int -> Lg.t

(** [ppar a b n] returns the parallel composition of link graphs [a] and [b].
    [n] is the number of nodes of [a].*)
val ppar : Lg.t -> Lg.t -> int-> Lg.t

(** [comp a b n] returns the composition of link graphs [a] and [b].
    [n] is the number of nodes of [a].
    @raise FACES_MISMATCH when names in the mediating interfaces differ *)
val comp : Lg.t -> Lg.t -> int -> Lg.t

(** {6 Predicates} *)
                                                                        
(** [is_id l] is true when link graph [l] is an identity, false othewise. *)
val is_id : Lg.t -> bool

(** [is_mono l] is true when link graph [l] is monomorphic, false otherwise. *)
val is_mono : Lg.t -> bool

(** [is_epi l] is true when link graph [l] is epimorphic, false otherwise. *)
val is_epi: Lg.t -> bool

(** [is_guard l] is true when no edges in link graph [l] have both inner and
    outer names. *)
val is_guard: Lg.t -> bool    

(** {6 Decompositions} *)

(** [decomp t p i_v i_e i_c i_d] builds the decomposition of target [t] given 
    pattern [p], isomorphism [i_v], relation [i_e], and isos from nodes in [t]
    to nodes of [c] and [d], respectively. Pattern [p] is assumed epi and mono,
    [i_v] is from nodes in [p] to nodes in [t] and [i_e] is from edges in [p] 
    to edges in [t]. Isos [i_c] and [i_d] are obtained by {!Place.decomp}. The
    results are link graph [c], [d] and [id].*)
val decomp : Lg.t -> Lg.t -> Base.Iso.t -> Base.Iso.t -> Base.Iso.t -> Base.Iso.t -> Lg.t * Lg.t * Lg.t

(** [levels l ps] returns the levels of link graph [l]. List [ps] is obtained 
    by {!Place.levels}. The output is a wiring and a list of
    identities.*)
val levels : Lg.t -> Base.Ports.t list -> Lg.t * Lg.t list

(** {6 Matching constraints} *)

(** [closed_edges l]  returns a pair [(h, i)] in which [h] is the set of 
    closed edges of [l] and [i] is an iso between the indices of [l] and [h]. *)
val closed_edges : Lg.t -> Lg.t

(* Closed edges in the pattern can be matched only to closed edges in the 
   target. The output is a list of clauses and a list of blocked rows 
   (list of negated literals). *)
val match_edges : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  Cnf.clause list * Cnf.clause list

(* Ports in matched closed edges have to be isomorphic *)
val match_ports : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  Cnf.clause list -> Cnf.clause list list

(*val match_links : Lg.t -> Lg.t -> Base.Iso.t * Base.Iso.t*)

val match_peers : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t ->
  int * int * (Cnf.b_clause list * Cnf.clause list) list * Cnf.clause list

(*val match_link_pairs : Lg.t -> Lg.t -> Base.Nodes.t -> Base.Nodes.t -> (int * int) list*)

(*val is_match_valid : Lg.t -> Lg.t -> Base.Iso.t -> bool*) 

(*(** Returns the pairs of nodes that can't be matched.*)
 val match_open : Lg.t -> Lg.t -> Base.Iso.t*)

(** Returns a par of isos and a list of clauses: nodes and close edges that can't be 
    matched, respectively. A clause is a list of pairs.*)
(*val match_close : Lg.t -> Lg.t -> 
  (int * int * int * int) list * Base.Iso.t * (int * int) list list*)

(**/**)
