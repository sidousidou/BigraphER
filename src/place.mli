(** This module provides operations on place graphs.

    @author Michele Sevegnani *)

(** The type of boolean matrices. *)
type m = Sparse.t

(** The type of place graphs. *)
type t = {
  r : int; (** Number of regions *)
  n : int; (** Number of nodes *)
  s : int; (** Number of sites *)
  rn : m;  (** Boolean adjacency matrix regions X nodes *)
  rs : m;  (** Boolean adjacency matrix regions X sites *)
  nn : m;  (** Boolean adjacency matrix nodes X nodes *)
  ns : m;  (** Boolean adjacency matrix nodes X sites *)
}

(** [to_string p] returns a string representation of place graph [p]. *)
val to_string : t -> string

(** Pretty printer. *)
val pp : Format.formatter -> t -> unit

(** Computes the representation of a boolean matrix as a list of edges. *)
val edges : m -> (int * int) list

(** Compute the number of edges in the DAG. *)
val size : t -> int

(** [parse r n s lines] computes a place graph with [r] regions, [n] nodes and [s]
    sites. Each element in [lines] is a string in the same format of the output
    of {!val:to_string}. 

    @raise Invalid_argument if the arguments do not specify a valid place
    graph. *)
val parse : regions:int -> nodes:int -> sites:int -> string list -> t

(** [get_dot p] returns three strings expressing place graph [p] in dot format.
    The first two elements encode regions and sites shapes, the third encodes the
    node ranks and the fourth represents the adjacency matrix. *)
val get_dot : t -> string * string * string * string

(** Apply an isomorphism *)
val apply : Iso.t -> t -> t

(** [parse_placing l r] returns the placing with [r] regions defined by list [l]
    in which each element is a site's parent set. *)
val parse_placing : int list list -> int -> t

(** Compute the set of nodes with no children. *)					      
val leaves : t -> IntSet.t

(** Compute the set of nodes with no parents. *)		     
val orphans : t -> IntSet.t

(** {2 Elementary place graphs} *)

(** [elementary_id m] computes a place graph in the form of an identity of
    [m]. *)
val elementary_id : int -> t

(** [id0] is the empty place graph. *)
val id0 : t

(** [elementary_merge m] computes an elementary place graph formed by one region
    containing [m] sites. *)
val elementary_merge : int -> t

(** [elementary_split m] computes an elementary place graph formed by one site
    shared by [m] regions. *)
val elementary_split : int -> t

(** [zero] is the place graph formed by an orphaned site. *)
val zero : t

(** [one] is the place graph formed by an idle region. *)
val one : t

(** [elementary_sym m n] computes an place graph consisting of a symmetry
    between [m] and [n]. *)
val elementary_sym : int -> int -> t

(**  An ion. *)
val elementary_ion : t

(** {2 Comparison} *)

(** [equal_placing a b] returns [true] if placings [a] and [b] are equal.
    Inputs are assumed to be valid placing. No check is performed. *)
val  equal_placing : t -> t -> bool

(** [compare_placing a b] compares placings [a] and [b]. *)
val compare_placing : t -> t -> int

(** Equality for {!type:bmatrix}. *)
(*val equal_bmatrix : bmatrix -> bmatrix -> bool

(** Comparison for {!type:bmatrix}. *)
val compare_bmatrix : bmatrix -> bmatrix -> int

(** Return the number of edges in a {!type:bmatrix}. *)
  val entries_bmatrix : bmatrix -> int*)

(** {2 Operations} *)

(** Raised when a composition between two incompatible place graphs is
    attempted. The two integers are the number of sites and regions involved in
    the composition, respectively. *)
exception COMP_ERROR of (int * int)

(** [tens p0 p1] returns the tensor product of place graphs [p0] and [p1]. *)
val tens : t -> t -> t

(** Same as {!val:tens} but on a list of place graphs. *)			 
val tens_of_list : t list -> t

(** [comp p0 p1] returns the composition of place graphs [p0] and [p1].
    
    @raise COMP_ERROR when mediating interfaces mismatch. *)
val comp : t -> t -> t

(** {2 Predicates} *)

(** [is_id p] returns [true] if place graph [p] is an identity, [false]
    otherwise. *)
val is_id : t -> bool

(** Test for the absence of nodes. *)
val is_plc : t -> bool

(** [is_ground p] is [true] if place graph [p] has no sites, [false]
    otherwise. *)
val is_ground : t -> bool

(** [is_mono p] is [true] if place graph [p] is monomorphic, [false]
    otherwise. A place graph is monomorphic if no two sites are siblings and no
    site is an orphan. *)
val is_mono : t -> bool

(** [is_epi p] is [true] if place graph [p] is epimorphic, [false] otherwise. A
    place graph is epimorphic if no region is idle and no two regions are
    partners. *)
val is_epi : t -> bool

(** [is_guard p] is [true] if place graph [p] is guarded, [false] otherwise. A
    place graph is guarded if no region has sites as children. *)
val is_guard : t -> bool

(** {2 Decompositions} *)

(** [decomp t p i] computes the decomposition of target [t] given pattern [p]
    and node isomorphism [i] from [p] to [t]. Pattern [p] is assumed epi and
    mono. The result tuple [(c, id, d, iso_c, iso_d)] is formed by context [c],
    identity [id], parameter [d], and nodes in [c] and [d] expressed as rows of
    [t]. *)
val decomp : target:t -> pattern:t -> Iso.t -> t * t * t * Iso.t * Iso.t

(** Raised when a place graph cannot be decomposed into prime components. The
    first element is a set of shared nodes and the second a set of shared
    sites. *)
exception NOT_PRIME

(** Compute the prime components ({e i.e.} place graphs with one region) of a
    place graph. The original node numbering is returned in the form of an
    isomorphism. 

    @raise NOT_PRIME when some region is shared *)
val prime_components : t -> (t * Iso.t) list

(** Compute the decomposition [D = D' X D_id].

     @raise NOT_PRIME when some region is shared *)					      
val decomp_d : t -> int -> t * t * Iso.t * Iso.t

(** {2 Matching constraints} *)

(** Raised when a node in the pattern cannot be matched to any node in the
    target. *)
exception NOT_TOTAL

(** [match_list t p n_t n_p] computes constraints to match edges in pattern [p]
    with compatible edges in target [t]. [n_t] and [n_p] are the node sets of
    [t] and [p], respectively.

    @raise NOT_TOTAL when there are nodes in the pattern that cannot be
    matched. *)
val match_list : target:t -> pattern:t -> n_t:Nodes.t -> n_p:Nodes.t ->
  (Cnf.clause * Cnf.b_clause list) list * Cnf.clause list * IntSet.t

(** [match_leaves t p n_t n_p] computes constraints to match the leaves 
    ({e i.e.} nodes without children) in [p] with those in [t]. [n_t] and [n_p]
     are defined as in {!val:match_list}.

    @raise NOT_TOTAL when there are leaves in the pattern that cannot be
    matched. *)
val match_leaves : target:t -> pattern:t -> n_t:Nodes.t -> n_p:Nodes.t -> 
  Cnf.clause list * IntSet.t

(** Dual of {!val:match_leaves}.

    @raise NOT_TOTAL when there are orphans in the pattern that cannot be matched. *)
val match_orphans : target:t -> pattern:t -> n_t:Nodes.t ->n_p: Nodes.t -> 
  Cnf.clause list * IntSet.t

(** Compute constraints to match regions. Arguments are as in
    {!val:match_list}. Only controls and degrees are checked. *)
val match_regions : target:t -> pattern:t -> n_t:Nodes.t -> n_p:Nodes.t ->
  Cnf.clause list * IntSet.t

(** Dual of {!val:match_regions}. *)		      
val match_sites : target:t -> pattern:t -> n_t:Nodes.t -> n_p:Nodes.t ->
  Cnf.clause list * IntSet.t

(** Compute constraints to block matches between unconnected pairs of nodes with
    sites and nodes with regions. *)
val match_trans : target:t -> pattern:t -> Cnf.clause list

(** [check_match t p trans iso] checks if [iso] from [p] to [t] is a valid match. *)
val check_match : target:t -> pattern:t -> m -> Iso.t -> bool

(** Compute constraints for equality: regions must have children with the same
    controls. *)
val match_region_nodes : t -> t -> Nodes.t -> Nodes.t -> 
  Cnf.clause list * IntSet.t

(** Dual of {!val:match_region_nodes}. *)		      
val match_nodes_sites : t -> t -> Nodes.t -> Nodes.t -> 
  Cnf.clause list * IntSet.t

(** Compute constraints for equality. Similar to {!val:match_list}. *)		      
val match_list_eq : t -> t -> Nodes.t -> Nodes.t ->
  (Cnf.clause * Cnf.b_clause list) list * Cnf.clause list * IntSet.t

(** Compute the outer degree of the regions of a place graph. *)
val deg_regions : t -> int list

(** Compute the inner degree of the sites of a place graph. *)			  
val deg_sites : t -> int list

(** Compute the transitive closure of the nodes X nodes matrix of a place
    graph. *)
val trans : t -> m

(**/**)
