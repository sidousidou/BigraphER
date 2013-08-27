(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on place graphs.
@author Michele Sevegnani *)

(** The type of place graphs.*)
type pg = {
  r : int; (** Number of roots *)
  n : int; (** Number of nodes *)
  s : int; (** Number of sites *)
  rn : Sparse.bmatrix; (** Boolean adjacency matrix roots X nodes *)
  rs : Sparse.bmatrix; (** Boolean adjacency matrix roots X sites *)
  nn : Sparse.bmatrix; (** Boolean adjacency matrix nodes X nodes *)
  ns : Sparse.bmatrix; (** Boolean adjacency matrix nodes X sites *)
 }
	
(** Raised when a composition between two incompatible place graphs is 
    attempted. *)
exception COMP_ERROR of (int * int)

(** [to_string p] returns a string representation of place graph [p]. *)
val to_string : pg -> string

(** [snf_of_placing p] returns the normal form of  a placing. *)
val snf_of_placing :  pg -> string

(** [get_dot p] returns three strings expressing place graph [p] in dot format.
    The first two elements encode roots and sites shapes, the third
    encodes the node ranks and the fourth represents the adjacency matrix. *)
val get_dot : pg -> string * string * string * string

(** [match_list t p] returns a list of non isomorphic nodes. Every element
    takes the form [(i,l,j,k)] with [i] and [l] nodes of [p] and [j] and [k]
    nodes of [t]. All indices are columns.*)
val match_list : pg -> pg -> (int * int * int * int) list

(** [apply_iso i p] returns a fresh place graph obtained by applying 
    isomorphism [i] to [p].*)
val apply_iso : Base.Iso.t -> pg -> pg

(** [parse_placing l r] returns the placing with [r] roots defined by list
    [l] in which each element is a site's parent set.*)
val parse_placing : int list list -> int -> pg

(** {6 Elementary place graphs} *)

(** [elementary_id m] creates a place graph in the form of an identity of [m].*)
val elementary_id : int -> pg

(** [id0] is the empty place graph.*)
val id0: pg

(** [elementary_merge m] creates an elementary place graph formed by one root
    containing [m] sites. *)
val elementary_merge : int -> pg

(** [elementary_split m] creates an elementary place graph formed by one site
    shared by [m] roots. *)
val elementary_split : int -> pg

(** [zero] is the place graph formed by an orphaned site.*)
val zero: pg

(** [one] is the place graph formed by an idle root.*)
val one: pg

(** [elementary_sym m n] creates an place graph consisting of a symmetry
    between [m] and [n]. *)
val elementary_sym : int -> int -> pg

(** [elementary_ion] creates a ion.*)
val elementary_ion : pg

(** [elementary_ions n] creates [n] ions in parallel. *)
val elementary_ions: int -> pg

(** {6 Comparison} *)

(** [equal_placing a b] returns [true] if placings [a] and [b] are equal. 
Inputs are assumed to be valid placing. No check performed. *)
val  equal_placing : pg -> pg -> bool

(** [compare_placing a b] compares placings [a] and [b].*)
val compare_placing : pg -> pg -> int

(** {6 Operations} *)

(** [tens p0 p1] returns the tensor product of place graphs [p0] and [p1]. *)
val tens : pg -> pg -> pg

(** [comp p0 p1] returns the composition of place graphs [p0] and [p1].
    @raise COMP_ERROR when mediating interfaces mismatch*)
val comp : pg -> pg -> pg

(** {6 Predicates} *)
                                                                        
(** [is_id p] returns [true] if place graph [p] is an identity, [false]
    otherwise.*)
val is_id : pg -> bool

(** Test for the absence of nodes.*)
val is_plc : pg -> bool

(** [is_mono p] returns [true] if place graph [p] is monomorphic, [false]
    otherwise.*)
val is_mono : pg -> bool

(** [is_epi p] returns [true] if place graph [p] is epimorphic, [false]
    otherwise.*)
val is_epi : pg -> bool

(** [is_guard p] returns [true] if place graph [p] is guarded, [false]
    otherwise. *)
val is_guard : pg -> bool

(** {6 Decompositions} *)

(** [decomp t p i] builds the decomposition of target [t] given pattern [p]
    and node isomorphism [i] from [p] to [t]. Pattern [p] is assumed epi and 
    mono. The result is context [c], identity [id], parameter [d], and nodes 
    in [c] and [d] expressed as rows of [t]. *)
val decomp : pg -> pg -> Base.Iso.t -> pg * pg * pg * Base.Iso.t * Base.Iso.t

(** [levels p] returns the levels of place graph [p]. The first component is the
    top placing, while the second is a list of triples. The first element is
    a set of ions, the second component is the size of the level's identity,
    and the third is the level's placing.
    *)
val levels : pg -> pg * (Base.IntSet.t * int * pg) list

(** {6 Misc} *)

(** [match_leaves t p] computes all the pairs of nodes [(i,j)] where [i] is a leaf
    in [p] and [j] is not a leaf in [t].*)
val match_leaves : pg -> pg -> Base.Iso.t

(** Dual of {!Place.match_leaves}. *)
val match_orphans : pg -> pg -> Base.Iso.t

(** [match_sites t p] computes all the pairs of nodes [(i,j)] where [i] and [j]
    have a different number of siblings. *)
val match_sites : pg -> pg -> Base.Iso.t

(** Dual of {!Place.match_sites}.*)
val match_roots : pg -> pg -> Base.Iso.t

val match_root_nodes : pg -> pg -> (int * int) list

val match_nodes_sites : pg -> pg -> (int * int) list

val match_roots_sites : pg -> pg -> bool

val compare_roots_sites : pg -> pg -> int

(** [is_match_valid t p t_trans i] check if iso [i] from pattern [p] to target [t] is
    valid. [t_trans] is the transitive closure of [t]. *)
val is_match_valid : pg -> pg -> Sparse.bmatrix -> Base.Iso.t -> bool

(** Computes the number of edges in the DAG. *)
val edges : pg -> int

(**/**)
