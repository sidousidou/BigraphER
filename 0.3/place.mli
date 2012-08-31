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
  m : Matrix.bmatrix; (** Boolean adjacency matrix of size [r+n] x [n+s] *)
 }
	
(** Raised when a composition between two incompatible place graphs is 
    attempted. *)
exception COMP_ERROR of (int * int)

(** [string_of_pg p] returns a string representation of place graph [p]. *)
val string_of_pg : pg -> string

(** [get_dot p] returns three strings expressing place graph [p] in dot format.
    The first two elements encode roots and sites shapes, the third
    encodes the node ranks and the fourth represents the adjacency matrix. *)
val get_dot : pg -> string * string * string * string

(* [match_string p] returns a triple which elements are the number of roots,
    the number of sites and the adjacency matrix, respectively.*)
(*val match_string : pg -> int * int * string*)

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
val levels : pg -> pg * (Base.Int_set.t * int * pg) list

(* REQUIRED for Big.fix_bug_roots and Big.fix_bug_sites *)

(*(** [children_of_roots p] returns a set of nodes which are children of a root.
 Nodes are counted from 0 to n-1. *)
val children_of_roots : pg -> Base.Int_set.t

(** [parents_of_sites p] returns a set of nodes which are parents of a site.
 Nodes are counted from 0 to n-1. *)
val parents_of_sites : pg -> Base.Int_set.t

(** [siblings p i] returns the set of nodes having a parent in common with node
 [i]. Nodes and [i] are counted from 0 to n-1. *)
val siblings : pg -> int -> Base.Int_set.t

(** [partners p i] returns the set of nodes having a child in common with node
 [i]. Nodes and [i] are counted from 0 to n-1. *)
val partners : pg -> int -> Base.Int_set.t*)

(**/**)
