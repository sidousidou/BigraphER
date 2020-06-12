(** This module provides operations on place graphs.

    @author Michele Sevegnani *)

type m = Sparse.t
(** The type of boolean matrices. *)

type t = {
  r : int;  (** Number of regions *)
  n : int;  (** Number of nodes *)
  s : int;  (** Number of sites *)
  rn : m;  (** Boolean adjacency matrix regions X nodes *)
  rs : m;  (** Boolean adjacency matrix regions X sites *)
  nn : m;  (** Boolean adjacency matrix nodes X nodes *)
  ns : m;  (** Boolean adjacency matrix nodes X sites *)
}
(** The type of place graphs. *)

val to_string : t -> string
(** [to_string p] returns a string representation of place graph [p]. *)

val pp : Format.formatter -> t -> unit
(** Pretty printer. *)

val edges : m -> (int * int) list
(** Computes the representation of a boolean matrix as a list of edges. *)

val size : t -> int
(** Compute the number of edges in the DAG. *)

val parse : regions:int -> nodes:int -> sites:int -> string list -> t
(** [parse r n s lines] computes a place graph with [r] regions, [n] nodes
    and [s] sites. Each element in [lines] is a string in the same format of
    the output of {!val:to_string}.

    @raise Invalid_argument if the arguments do not specify a valid place
    graph. *)

val get_dot : t -> string * string * string * string
(** [get_dot p] returns three strings expressing place graph [p] in dot
    format. The first two elements encode regions and sites shapes, the third
    encodes the node ranks and the fourth represents the adjacency matrix. *)

val apply : Iso.t -> t -> t
(** Apply an isomorphism *)

val parse_placing : int list list -> int -> t
(** [parse_placing l r] returns the placing with [r] regions defined by list
    [l] in which each element is a site's parent set. *)

val leaves : t -> IntSet.t
(** Compute the set of nodes with no children. *)

val orphans : t -> IntSet.t
(** Compute the set of nodes with no parents. *)

(** {2 Elementary place graphs} *)

val elementary_id : int -> t
(** [elementary_id m] computes a place graph in the form of an identity of
    [m]. *)

val id0 : t
(** [id0] is the empty place graph. *)

val elementary_merge : int -> t
(** [elementary_merge m] computes an elementary place graph formed by one
    region containing [m] sites. *)

val elementary_split : int -> t
(** [elementary_split m] computes an elementary place graph formed by one
    site shared by [m] regions. *)

val zero : t
(** [zero] is the place graph formed by an orphaned site. *)

val one : t
(** [one] is the place graph formed by an idle region. *)

val elementary_sym : int -> int -> t
(** [elementary_sym m n] computes an place graph consisting of a symmetry
    between [m] and [n]. *)

val elementary_ion : t
(** An ion. *)

(** {2 Comparison} *)

val equal_placing : t -> t -> bool
(** [equal_placing a b] returns [true] if placings [a] and [b] are equal.
    Inputs are assumed to be valid placing. No check is performed. *)

val compare_placing : t -> t -> int
(** [compare_placing a b] compares placings [a] and [b]. *)

(** {2 Operations} *)

exception COMP_ERROR of (int * int)
(** Raised when a composition between two incompatible place graphs is
    attempted. The two integers are the number of sites and regions involved
    in the composition, respectively. *)

val tens : t -> t -> t
(** [tens p0 p1] returns the tensor product of place graphs [p0] and [p1]. *)

val tens_of_list : t list -> t
(** Same as {!val:tens} but on a list of place graphs. *)

val comp : t -> t -> t
(** [comp p0 p1] returns the composition of place graphs [p0] and [p1].

    @raise COMP_ERROR when mediating interfaces mismatch. *)

(** {2 Predicates} *)

val is_id : t -> bool
(** [is_id p] returns [true] if place graph [p] is an identity, [false]
    otherwise. *)

val is_plc : t -> bool
(** Test for the absence of nodes. *)

val is_ground : t -> bool
(** [is_ground p] is [true] if place graph [p] has no sites, [false]
    otherwise. *)

val is_mono : t -> bool
(** [is_mono p] is [true] if place graph [p] is monomorphic, [false]
    otherwise. A place graph is monomorphic if no two sites are siblings and
    no site is an orphan. *)

val is_epi : t -> bool
(** [is_epi p] is [true] if place graph [p] is epimorphic, [false] otherwise.
    A place graph is epimorphic if no region is idle and no two regions are
    partners. *)

val is_guard : t -> bool
(** [is_guard p] is [true] if place graph [p] is guarded, [false] otherwise.
    A place graph is guarded if no region has sites as children. *)

(** {2 Decompositions} *)

val decomp : target:t -> pattern:t -> Iso.t -> t * t * t * Iso.t * Iso.t
(** [decomp t p i] computes the decomposition of target [t] given pattern [p]
    and node isomorphism [i] from [p] to [t]. Pattern [p] is assumed epi and
    mono. The result tuple [(c, id, d, iso_c, iso_d)] is formed by context
    [c], identity [id], parameter [d], and nodes in [c] and [d] expressed as
    rows of [t]. The decomposition is with respect to the minimal parameter *)

exception NOT_PRIME
(** Raised when a place graph cannot be decomposed into prime components. The
    first element is a set of shared nodes and the second a set of shared
    sites. *)

val prime_components : t -> (t * Iso.t) list
(** Compute the prime components ({e i.e.} place graphs with one region) of a
    place graph. The original node numbering is returned in the form of an
    isomorphism.

    @raise NOT_PRIME when some region is shared *)

val decomp_d : t -> int -> t * t * Iso.t * Iso.t
(** Compute the decomposition [D = D' X D_id].

    @raise NOT_PRIME when some region is shared *)

val trans : t -> m
(** Compute the transitive closure of the nodes X nodes matrix of a place
    graph. *)

(**/**)
