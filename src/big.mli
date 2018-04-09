(** This module provides operations on bigraphs.
    @author Michele Sevegnani *)

(** The type of bigraphs.*)
type t = {
  p : Place.t;      (** Place graph *)
  l : Link.Lg.t;    (** Link graph  *)
  n : Nodes.t;      (** Node set    *)
}

(** The type of interfaces.*)
type inter = Inter of int * Link.Face.t
(** [Inter (i,n)] creates an interface with ordinal [i] and names [n]. *)

(** {2 Exceptions} *)

(** Raised when the composition in a sharing expression fails. *)
exception SHARING_ERROR

(** Raised when the composition fails. *)
exception COMP_ERROR of inter * inter

(** Raised when the arity of a control does not match the cardinality of a
    face. The first element is the arity while the second is the mismatching
    face. *)
exception CTRL_ERROR of int * Link.Face.t

(** Raised when a {!type:Iso.t} is not total. The first element is the
    cardinality of the domain while the second is the cardinality of the
    isomorphism's domain of definition. *)
exception ISO_ERROR of int * int

(** Raised when there are no matches.*)
exception NO_MATCH

(** Raised when the matching pattern has no nodes. *)
exception NODE_FREE

(** {2 Functions on interfaces} *)

(** Equality over interfaces. *)
val inter_equal : inter -> inter -> bool

(** Compute the string representation of an interface. *)
val string_of_inter: inter -> string

(** Compute the ordinal of an interface. *)
val ord_of_inter : inter -> int

(** Compute the face ({e i.e.} name-set) of an interface. *)
val face_of_inter : inter -> Link.Face.t

(** {2 Functions on bigraphs} *)

(** Compute the string representation of a bigraph. Example:

    ["{(0, A:3),(1, A:3)}
 2 2 0
 10
 10
 01
 00
 ({}, {}, {(0, 1), (1, 2)})
 ({}, {}, {(0, 2), (1, 1)})"] *)
val to_string: t -> string

(** Parse a string produced by {!val:Big.to_string} to a value of type
    {!type:Big.t}.

    @raise Invalid_argument if the string cannot be parsed. *)
val of_string: string -> t

(** Parse a bigraph. Example input format:

    ["2 2 0 2
 A A
 10
 10
 01
 00
 1 1 2 f
 1 2 2 f"]

    The first line specifies the number of regions, nodes, sites, and links in
    the displayed order. The second line lists the controls of the nodes. It
    follows the adjacency matrix for the place graph and a list of links. A link
    is open (closed) if the corresponding line terminates with [t] ([f]). Note
    this example corresponds to bigraph shown in the documentation of
    {!val:Big.to_string}. 

    @raise Invalid_argument if the input cannot be parsed. *)
val parse : string -> t

(** [to_dot b n] compute the string expressing bigraph [b] named [n] in 
    [dot] format. *)
val to_dot : t -> string -> string

(** [inner b] computes the inner face of bigraph [b]. *)
val inner : t -> inter

(** [outer b] computes the outer face of bigraph [b]. *)
val outer : t -> inter

(** [apply i b] applies isomorphism [i] to bigraph [b]. *)
val apply : Iso.t -> t -> t

(** [placing l r f] computes a placing with [r] regions by parsing list [l]. The
    format of [l] is the same as the input for {!val:Place.parse_placing}.
    The link graph is the identity over face [f]. *)
val placing : int list list -> int -> Link.Face.t -> t

(** Compute the size of a bigraph as the sum of the cardinalities of the node
    set and edge set. *)
val size : t -> int

(** {2 Elementary bigraphs} *)

(** Identity over interface [i]. *)
val id : inter -> t

(** The empty identity.*)
val id_eps : t

(** Bigraph consisting of one region and [n] sites. *)
val merge : int -> t

(** Bigraph consisting of one site and [n] regions. *)
val split : int -> t

(**  Elementary bigraph consisting of one region. *)
val one : t

(** Elementary bigraphs consisting of one site. *)
val zero : t

(** Symmetry on interfaces [i] and [j]. *)
val sym : inter -> inter -> t

(** [ion ns c] computes an ion of control [c]. It's outer names are [ns]. No
    arity check is performed. *)
val ion : Link.Face.t -> Ctrl.t -> t

(** Same as {!Big.ion} but with arity check.

    @raise CONTROL_ERROR when the set of names has size different than the arity
    of [c]. *)
val ion_chk : Link.Face.t -> Ctrl.t -> t

(** Same as {!Big.ion} but without the site. *)
val atom : Link.Face.t -> Ctrl.t -> t

(** Same as {!Big.ion_chk} but without the site.

    @raise CONTROL_ERROR when the set of names has size different than the arity
    of [c]. *)
val atom_chk : Link.Face.t -> Ctrl.t -> t

(** [sub inner outer] computes a substitution where [inner] and [outer] are the
    inner and outer faces, respectively. *)
val sub : inner:Link.Face.t -> outer:Link.Face.t -> t

(** [closure f] computes the closure of interface [f].*)
val closure : Link.Face.t -> t

(** [intro f] computes an empty bigraph providing a fresh set of names [f]. This
    function is the dual of {!Big.closure}. *)
val intro : Link.Face.t -> t

(** {2 Operations} *)

(** [close f b] closes names in [f]. Example: [\x0 \x1 b]. *)
val close : Link.Face.t -> t -> t

(** [comp a b] computes the composition of bigraphs [a] and [b], 
    {%html:<i class="math">a &#x2218; b</i>%} using the algebraic 
    notation.

    @raise COMP_ERROR when the mediating interfaces do not match. *)
val comp : t -> t -> t

(** [comp_of_list bs] computes the composition of all the bigraphs in list
    [bs]. 

    @raise COMP_ERROR when the mediating interfaces do not match. *)
val comp_of_list : t list -> t

(** [comp_seq ~start ~stop f] computes the composition
    [f(start) * ... * f(stop - 1)].

    @raise COMP_ERROR when the mediating interfaces do not match. *)
val comp_seq : start:int -> stop:int -> (int -> t) -> t
  
(** [nest a b] computes the bigraph resulting from nesting bigraph [b] in 
    bigraph [a]. Common names are shared.

    @raise COMP_ERROR if composition cannot be performed. *)
val nest : t -> t -> t

(** [par a b] computes the merge product of bigraphs [a] and [b],
    {%html:<i class="math">a &#x2223; b</i>%} using the algebraic 
    notation. *)
val par : t -> t -> t

(** [par_of_list bs] computes the merge product of all the bigraphs in list
    [bs].*)
val par_of_list: t list -> t

(** [par_seq ~start ~stop f] computes the composition
    [f(start) | ... | f(stop - 1)]. *)
val par_seq : start:int -> stop:int -> (int -> t) -> t

(** [ppar a b] computes the parallel product of bigraphs [a] and [b],
    {%html:<i class="math">a &#x2225; b</i>%} using the algebraic 
    notation. *)
val ppar : t -> t -> t

(** [ppar_of_list bs] computes the parallel product of all the bigraphs in list
    [bs]. *)
val ppar_of_list: t list -> t

(** [ppar_seq ~start ~stop f] computes the composition
    [f(start) || ... || f(stop - 1)]. *)
val ppar_seq : start:int -> stop:int -> (int -> t) -> t

(** [rename inner outer b] renames the names in [in] to the names in [out]. The
    outer names in [b] not in [inner] are left untouched. *)
val rename : inner:Link.Face.t -> outer:Link.Face.t -> t -> t

(** [share f psi g] computes the bigraphs obtained by sharing bigraph [f] in
    bigraph [g] by using placing [psi].

    @raise SHARING_ERROR if [psi] is not a placing.
    @raise COMP_ERROR if one composition cannot be performed. *)
val share : t -> t -> t -> t

(** [tens a b] computes the tensor product of bigraphs [a] and [b], 
    {%html:<i class="math">a &otimes; b</i>%} using the algebraic 
    notation.

    @raise Link.NAMES_ALREADY_DEFINED when [a] and [b] have shared names. *)
val tens : t -> t -> t

(** [tens_of_list bs] computes the tensor product of all the bigraphs in list
    [bs]. 

    @raise Link.NAMES_ALREADY_DEFINED when two addends have shared names. *)
val tens_of_list : t list -> t
  
(** [tens_seq ~start ~stop f] computes the composition 
    [f(start) + ... + f(stop - 1)]. 

    @raise Link.NAMES_ALREADY_DEFINED when two addends have shared names. *)
val tens_seq : start:int -> stop:int -> (int -> t) -> t

(** {2 Predicates} *)

(** [is_id b] returns [true] if bigraph [b] is an identity, [false] otherwise. *)
val is_id : t -> bool

(** [is_plc b] returns [true] if bigraph [b] is a placing, [false] otherwise. *)
val is_plc : t -> bool

(** [is_wir b] returns [true] if bigraph [b] is a wiring, [false] otherwise. *)
val is_wir : t -> bool

(** [is_epi b] returns [true] if bigraph [b] is epimorphic, [false] otherwise.
    A bigraph is epimorphic if both its place graph and link graph are
    epimorphic. See {!val:Place.is_epi} and {!val:Link.is_epi}. *)
val is_epi : t -> bool

(** [is_mono b] returns [true] if bigraph [b] is monomorphic, [false]
    otherwise. A bigraph is monomorphic if both its place graph and link graph
    are monomorphic. See {!val:Place.is_mono} and {!val:Link.is_mono}. *)
val is_mono : t -> bool

(** [is_guard b] returns [true] if bigraph [b] is an guarded, [false]
    otherwise. A bigraph is guarded if both its place graph and link graph are
    guarded. See {!val:Place.is_guard} and {!val:Link.is_guard}. *)
val is_guard : t -> bool

(** [is_solid b] returns [true] if bigraph [b] is solid, [false] otherwise. A
    bigraph is solid if it is epimorphic, monomorphic, and guarded. See
    {!val:Big.is_epi}, {!val:Big.is_mono}, and {!val:Big.is_guard}. *)
val is_solid : t -> bool

(** [is_ground b] returns [true] if bigraph [b] is ground, [false] otherwise. A
    bigraph is ground if its place graph and link graph are both ground. See
    {!val:Place.is_ground} and {!val:Link.is_ground}. *)
val is_ground : t -> bool

(** {2 Decompositions} *)

(** [decomp t p i_n i_e f_e] computes the decomposition of target [t] given
    pattern [p], node isomorphism [i_n] and edge isomorphism [i_e]. The
    isomorphism are from [p] to [t]. The elements in the result are the context,
    the parameter and the identity of the decomposition. Argument [f_e] is a
    total function from links in the pattern to links in the target. *)
val decomp : target:t -> pattern:t -> i_n:Iso.t -> i_e:Iso.t ->
  Fun.t -> t * t * t

(** {2 Comparison} *)

(** [equal a b] returns [true] if bigraphs [a] and [b] are isomorphic, [false] otherwise. *)
val equal : t -> t -> bool

(** The type of bigraphs keys. *)			  
type big_key = int

(** Compute the key of a bigraph. The key is similar to a hash. Note
    that different bigraphs can have the same key. *)
val key : t -> big_key

(** Same as {!Big.equal} but with fewer checks prior to the SAT solver
    invocation. This function is intended to be used after equality over keys
    has already failed. *)
val equal_opt : t -> t -> bool

(** {2 Matching} *)

(** The type of occurrences: an isomorphism over nodes, an isomorphism
    over edges and a function over hyper-edges. *)
type occ = Iso.t * Iso.t * Fun.t

(** [occurs t p] returns [true] if pattern [p] occurs in target [t], [false]
    otherwise. *)
val occurs : target:t -> pattern:t ->  bool

(** [occurrence t p trans] returns a pair of isomorphisms [(i,j)] if pattern [p]
    occurs in target [t]. Isos [i] and [j] are defined over nodes and edges,
    respectively. Argument [trans] is the transitive closure of the induced
    graph of [t].

    @raise NODE_FREE when [p] has an empty node set. *)
val occurrence : target:t -> pattern:t -> Sparse.t -> occ option

(** [occurrences t p] returns a list of occurrences.

    @raise NODE_FREE when [p] has an empty node set. *)
val occurrences : target:t -> pattern:t -> occ list

(** [auto b] computes the non-trivial automorphisms of bigraph [b]. 

    @raise NODE_FREE when [p] has an empty node set. *)
val auto : t -> (Iso.t * Iso.t) list

(** [rewrite o s r0 r1 eta] computes a bigraph obtained by replacing the
    occurrence of [r0] (specified by occurrence [o]) in [s] with [eta r1], where
    [eta] is a valid (no check performed) instantiation map.

    @raise Place.NOT_PRIME when [b] is not prime decomposable. *)
val rewrite : occ -> s:t -> r0:t -> r1:t -> Fun.t option -> t

(**/**)
