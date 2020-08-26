(** This module provides operations on link graphs.

    @author Michele Sevegnani *)

(** {2 Faces} *)

(** The type of names. *)
type name = Name of string

(** This module provides set operations for faces. *)
module Face : sig
  type elt = name

  type t

  val add : elt -> t -> t

  val cardinal : t -> int

  val choose : t -> elt option

  val compare : t -> t -> int

  val diff : t -> t -> t

  val elements : t -> elt list

  val empty : t

  val equal : t -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val inter : t -> t -> t

  val is_empty : t -> bool

  val iter : (elt -> unit) -> t -> unit

  val max_elt : t -> elt option

  val mem : elt -> t -> bool

  val min_elt : t -> elt option

  val partition : (elt -> bool) -> t -> t * t

  val remove : elt -> t -> t

  val singleton : elt -> t

  val split : elt -> t -> t * bool * t

  val subset : t -> t -> bool

  val union : t -> t -> t
end

(** {2 Ports} *)

(** This module implements multisets of nodes as maps. *)
module Ports : sig
  type t

  (** {2 Standard operations on maps} *)

  val choose : t -> (int * int) option

  val compare : (int -> int -> int) -> t -> t -> int

  val empty : t

  val equal : t -> t -> bool

  val filter : (int -> int -> bool) -> t -> t

  val fold : (int -> int -> 'b -> 'b) -> t -> 'b -> 'b

  val is_empty : t -> bool

  val iter : (int -> int -> unit) -> t -> unit

  val max_binding : t -> (int * int) option

  val min_binding : t -> (int * int) option

  (** {2 Additional functions} *)

  val add : int -> t -> t

  val apply : Iso.t -> t -> t
  (** [apply i p] applies isomorphism [i] to port set [p]. Elements of [p]
      not in the domain of [i] are dropped. *)

  val arity : t -> int -> int option

  val cardinal : t -> int

  val of_list : (int * int) list -> t

  val of_nodes : Nodes.t -> t
  (** [of_nodes ns] transform a set of nodes into a set of ports. *)

  val sum : t -> t -> t

  val to_IntSet : t -> IntSet.t
  (** [to_IntSet ps] returns a set of node identifiers form a set of ports. *)

  val to_string : t -> string
  (** [to_string s] gives the string representation of port set [s]. For
      example:

      ["\{(0, 0), (1, 3)\}"]. *)
end

(** {2 Link graphs} *)

type edg = {
  i : Face.t;  (** Inner face *)
  o : Face.t;  (** Outer face *)
  p : Ports.t;  (** Set of ports *)
}
(** The type of edges. *)

(** This module provides set operations for link graphs. *)
module Lg : sig
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

  val min_elt : t -> elt option

  val max_elt : t -> elt option

  val choose : t -> elt option

  val split : elt -> t -> t * bool * t
end

val parse_face : string list -> Face.t
(** [parse_face ns] computes a face starting from list of string names [ns]. *)

val string_of_face : Face.t -> string
(** [string_of_face f] computes the string representation of face [f]. *)

val pp_face : Format.formatter -> Face.t -> unit
(** Pretty printer. *)

val to_string : Lg.t -> string
(** [to_string l] computes the string representation of link graph [l]. *)

val pp : Format.formatter -> Lg.t -> unit
(** Pretty printer. *)

val of_string : string -> Lg.t
(** Opposite of {!val:Link.to_string}.

    @raise Invalid_argument if the input cannot be parsed. *)

val parse : links:string list -> nodes:string -> Lg.t * Nodes.t
(** Parse a list of strings. *)

val get_dot : Lg.t -> string * string * string * string
(** [get_dot l] computes a four-elements tuple encoding the dot
    representation of link graph [l]. The first two elements represent inner
    and outer names shape declarations. The third element represent the
    hyperedges shape declarations. The fourth element specifies the adjacency
    matrix. *)

val inner : Lg.t -> Face.t
(** [inner l] computes the inner face of link graph [l]. *)

val outer : Lg.t -> Face.t
(** [outer l] computes the outer face of link graph [l]. *)

val apply : Iso.t -> Lg.t -> Lg.t
(** [apply i l] computes a link graph obtained by applying isomorphism [i] to
    [l]. *)

(** {2 Elementary link graphs} *)

val elementary_sub : inner:Face.t -> outer:Face.t -> Lg.t
(** [elementary_sub inner outer] computes a substitution consisting of a
    single edge in which [inner] and [outer] are the inner and outer face,
    respectively. *)

val elementary_ion : Face.t -> Lg.t
(** [elementary_ion f] computes an elementary ion with outer face [f]. *)

val elementary_id : Face.t -> Lg.t
(** [elementary_id f] computes the identity over face [f], {e i.e.} one edge
    for each name in [f]. *)

val id_empty : Lg.t
(** [id_empty] is the empty link graph. *)

(** {2 Operations on link graphs} *)

exception NAMES_ALREADY_DEFINED of (Face.t * Face.t)
(** Raised when the tensor product between two incompatible link graphs
    cannot be performed. The first element is the set of inner common names
    while the second is the set of outer common names. *)

exception FACES_MISMATCH of (Face.t * Face.t)
(** Raised when a composition between two incompatible link graphs cannot be
    performed. *)

val tens : Lg.t -> Lg.t -> int -> Lg.t
(** [tens a b n] computes the tensor product of link graphs [a] and [b].
    Argument [n] is the number of nodes of [a].

    @raise NAMES_ALREADY_DEFINED when there are shared names. *)

val ppar : Lg.t -> Lg.t -> int -> Lg.t
(** [ppar a b n] computes the parallel composition of link graphs [a] and
    [b]. [n] is the number of nodes of [a]. *)

val comp : Lg.t -> Lg.t -> int -> Lg.t
(** [comp a b n] computes the composition of link graphs [a] and [b].
    Argument [n] is the number of nodes of [a].

    @raise FACES_MISMATCH when names in the mediating interfaces differ. *)

(** {2 Predicates} *)

val is_id : Lg.t -> bool
(** [is_id l] is [true] if link graph [l] is an identity, [false] otherwise. *)

val is_mono : Lg.t -> bool
(** [is_mono l] is [true] if link graph [l] is monomorphic, [false]
    otherwise. A link graph is monomorphic if every edge has at most one
    inner name. *)

val is_epi : Lg.t -> bool
(** [is_epi l] is [true] if link graph [l] is epimorphic, [false] otherwise.
    A link graph is epimorphic if no outer name is idle. *)

val is_ground : Lg.t -> bool
(** [is_ground l] is [true] if link graph [l] has no inner names, [false]
    otherwise. *)

val is_guard : Lg.t -> bool
(** [is_guard l] is [true] if no edges in link graph [l] have both inner and
    outer names, [false] otherwise. *)

val max_ports : Lg.t -> int
(** Compute the maximum number of ports in one edge of a link graph. *)

val cardinal_ports : Lg.t -> int list
(** Compute a sorted list with the cardinalities of the port sets. *)

val closed_edges : Lg.t -> int
(** Compute the number of closed edges. *)

val closed_edges_iso : Lg.t -> Lg.t * Iso.t
(** [closed_edges_iso l] computes the set of closed edges of link graph [l].
    The computed isomorphism maps edge indices of the new link graph to
    indices of edges in [l]. *)

(** {2 Decompositions} *)

val norm : Lg.t -> Lg.t * Lg.t
(** Normalise link graph [l] as follows: [l = omega o l'] where [omega] is a
    linking and [l'] is the same as [l] but with all links open. *)

val decomp :
  target:Lg.t ->
  pattern:Lg.t ->
  i_e:Iso.t ->
  i_c:Iso.t ->
  i_d:Iso.t ->
  Fun.t ->
  Lg.t * Lg.t * Lg.t
(** [decomp target pattern i_e i_c i_d f_e] computes the decomposition of
    [target] given [pattern], iso [i_e], and isos from nodes in [t] to nodes
    of [c] and [d], respectively. Argument [f_e] is a total function from
    links in the pattern to links in the target. Pattern [p] is assumed epi
    and mono and [i_e] is from edges in [p] to edges in [t]. Isos [i_c] and
    [i_d] are obtained by {!val:Place.decomp}. The results are link graph
    [c], [d] and [id]. *)

val prime_components : Lg.t -> Iso.t list -> Lg.t list
(** Compute the prime components of a link graph. See {!val:Place.decomp}. *)

(**/**)
