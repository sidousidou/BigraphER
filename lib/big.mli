(** This module provides operations on bigraphs.
    @author Michele Sevegnani *)

(** The type of bigraphs.*)
type bg = {
  p : Place.pg;     (** Place graph *)
  l : Link.Lg.t;    (** Link graph  *)
  n : Nodes.t;      (** Node set    *)
}

(** The type of interfaces.*)
type inter = Inter of int * Link.Face.t
(** [Inter (i,n)] creates an interface with ordinal [i] and names [n]. *)

(** {3 Exceptions} *)

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

(** Raised when an export error occurs. *)
exception EXPORT_ERROR of string

(** {3 Functions on interfaces} *)

(** Equality over interfaces. *)
val inter_equal : inter -> inter -> bool

(** Compute the string representation of an interface. *)
val string_of_inter: inter -> string

(** Compute the ordinal of an interface. *)
val ord_of_inter : inter -> int

(** Compute the face ({e i.e.} name-set) of an interface. *)
val face_of_inter : inter -> Link.Face.t

(** {3 Functions on bigraphs} *)

(** Compute the string representation of a bigraph. Example:

    [{(0, A:3),(1, A:3)}
2 2 0
10
10
01
00
({}, {}, {(0, 1), (1, 2)})
({}, {}, {(0, 2), (1, 1)})] *)
val to_string: bg -> string

(** Parse a string produced by {!val:Big.to_string} to a value of type
    {!type:Big.bg}.

    @raise Invalid_argument if the string cannot be parsed. *)
val of_string: string -> bg

(** Return a JSON representation of a bigraph. See {!val:Nodes.json_of_nodes},
    {!val:Place.json_of_place}, and {!val:Link.json_of_link} for example
    outputs.*)
val json_of_big: bg -> string

(** Parse a bigraph. Example input format:

    [2 2 0 2
A A
10
10
01
00
1 1 2 f
1 2 2 f]

    The first line specifies the number of regions, nodes, sites, and links in
    the displayed order. The second line lists the controls of the nodes. It
    follows the adjacency matrix for the place graph and a list of links. A link
    is open (closed) if the corresponding line terminates with [t] ([f]). Note
    this example corresponds to bigraph shown in the documentation of
    {!val:Big.to_string}. 

    @raise Invalid_argument if the input cannot be parsed. *)
val parse : string -> bg

(** [to_dot b i] compute the string expressing bigraph [b] named [i] in 
    [dot] format. *)
val to_dot : bg -> string -> string

(** [inner b] computes the inner face of bigraph [b]. *)
val inner : bg -> inter

(** [outer b] computes the outer face of bigraph [b]. *)
val outer : bg -> inter

(** [apply i b] applies isomorphism [i] to bigraph [b]. *)
val apply : Iso.t -> bg -> bg

(** [placing l r f] computes a placing with [r] roots by parsing list [l]. The
    format of [l] is the same as the input for {!val:Place.parse_placing}.
    The link graph is the identity over face [f]. *)
val placing : int list list -> int -> Link.Face.t -> bg

(** {3 Export functions} *)

(** Export to file the string representation in [txt] format of a bigraph. 

    @raise EXPORT_ERROR when an error occurs. *)
val write_txt : bg -> name:string -> path:string -> int
  
(** Export to file the string representation in [dot] format of a bigraph. 

    @raise EXPORT_ERROR when an error occurs. *)
val write_dot : bg -> name:string -> path:string -> int

(** Export to file the string representation in [svg] format of a bigraph.

    @raise EXPORT_ERROR when an error occurs. *)
val write_svg : bg -> name:string -> path:string -> int

(** Export to file the string representation in [json] format of a bigraph.

    @raise EXPORT_ERROR when an error occurs. *)
val write_json : bg -> name:string -> path:string -> int

(** {3 Elementary bigraphs} *)

(** Identity over interface [i]. *)
val id : inter -> bg

(** The empty identity.*)
val id_eps : bg

(** Bigraph consisting of one root and [n] sites. *)
val merge : int -> bg

(** Bigraph consisting of one site and [n] roots. *)
val split : int -> bg

(**  Elementary bigraph consisting of one root. *)
val one : bg

(** Elementary bigraphs consisting of one site. *)
val zero : bg

(** Symmetry on interfaces [i] and [j]. *)
val sym : inter -> inter -> bg

(** [ion ns c] computes an ion of control [c]. It's outer names are [ns]. No
    arity check is performed. *)
val ion : Link.Face.t -> Ctrl.t -> bg

(** Same as {!Big.ion} but with arity check.

    @raise CONTROL_ERROR when the set of names has size different than the arity
    of [c]. *)
val ion_chk : Link.Face.t -> Ctrl.t -> bg

(** Same as {!Big.ion} but without the site. *)
val atom : Link.Face.t -> Ctrl.t -> bg

(** Same as {!Big.ion_chk} but without the site.

    @raise CONTROL_ERROR when the set of names has size different than the arity
    of [c]. *)
val atom_chk : Link.Face.t -> Ctrl.t -> bg

(** [sub inner outer] computes a substitution where [inner] and [outer] are the
    inner and outer faces, respectively. *)
val sub : inner:Link.Face.t -> outer:Link.Face.t -> bg

(** [closure f] computes the closure of interface [f].*)
val closure : Link.Face.t -> bg

(** [intro f] computes an empty bigraph providing a fresh set of names [f]. This
    function is the dual of {!Big.closure}. *)
val intro : Link.Face.t -> bg

(** {3 Operations} *)

(** [tens a b] computes the tensor product of bigraphs [a] and [b].

    @raise Link.NAMES_ALREADY_DEFINED when [a] and [b] have shared names. *)
val tens : bg -> bg -> bg

(** [comp a b] computes the composition of bigraphs [a] and [b].

    @raise COMP_ERROR when the mediating interfaces do not match. *)
val comp : bg -> bg -> bg

(** [ppar a b] computes the parallel product of bigraphs [a] and [b]. *)
val ppar : bg -> bg -> bg

(** [ppar_of_list bs] computes the parallel product of all the bigraphs in list
    [bs]. *)
val ppar_of_list: bg list -> bg

(** [par a b] computes the merge product of bigraphs [a] and [b]. *)
val par : bg -> bg -> bg

(** [par_of_list bs] computes the merge product of all the bigraphs in list
    [bs].*)
val par_of_list: bg list -> bg

(** [nest a b] computes the bigraph resulting from nesting bigraph [b] in 
    bigraph [a]. Common names are shared.

    @raise COMP_ERROR if composition cannot be performed. *)
val nest : bg -> bg -> bg

(** [share f psi g] computes the bigraphs obtained by sharing bigraph [f] in
    bigraph [g] by using placing [psi].

    @raise SHARING_ERROR if [psi] is not a placing.
    @raise COMP_ERROR if one composition cannot be performed. *)
val share : bg -> bg -> bg -> bg

(** [close f b] closes names in [f]. Example: [\x0 \x1 b]. *)
val close : Link.Face.t -> bg -> bg

(** [rename inner outer b] renames the names in [in] to the names in [out]. The
    outer names in [b] not in [inner] are left untouched. *)
val rename : inner:Link.Face.t -> outer:Link.Face.t -> bg -> bg
  
(** {3 Predicates} *)

(** [is_id b] returns [true] if bigraph [b] is an identity, [false] otherwise. *)
val is_id : bg -> bool

(** [is_plc b] returns [true] if bigraph [b] is a placing, [false] otherwise. *)
val is_plc : bg -> bool

(** [is_wir b] returns [true] if bigraph [b] is a wiring, [false] otherwise. *)
val is_wir : bg -> bool

(** [is_epi b] returns [true] if bigraph [b] is epimorphic, [false] otherwise.
    A bigraph is epimorphic if both its place graph and link graph are
    epimorphic. See {!val:Place.is_epi} and {!val:Link.is_epi}. *)
val is_epi : bg -> bool

(** [is_mono b] returns [true] if bigraph [b] is monomorphic, [false]
    otherwise. A bigraph is monomorphic if both its place graph and link graph
    are monomorphic. See {!val:Place.is_mono} and {!val:Link.is_mono}. *)
val is_mono : bg -> bool

(** [is_guard b] returns [true] if bigraph [b] is an guarded, [false]
    otherwise. A bigraph is guarded if both its place graph and link graph are
    guarded. See {!val:Place.is_guard} and {!val:Link.is_guard}. *)
val is_guard : bg -> bool

(** [is_solid b] returns [true] if bigraph [b] is solid, [false] otherwise. A
    bigraph is solid if it is epimorphic, monomorphic, and guarded. See
    {!val:Big.is_epi}, {!val:Big.is_mono}, and {!val:Big.is_guard}. *)
val is_solid : bg -> bool

(** [is_ground b] returns [true] if bigraph [b] is ground, [false] otherwise. A
    bigraph is ground if its place graph and link graph are both ground. See
    {!val:Place.is_ground} and {!val:Link.is_ground}. *)
val is_ground : bg -> bool

(** {3 Decompositions} *)

(** [decomp t p i_n i_e f_e] computes the decomposition of target [t] given
    pattern [p], node isomorphism [i_n] and edge isomorphism [i_e]. The
    isomorphism are from [p] to [t]. The elements in the result are the context,
    the parameter and the identity of the decomposition. Argument [f_e] is a
    total function from links in the pattern to links in the target. *)
val decomp : target:bg -> pattern:bg -> i_n:Iso.t -> i_e:Iso.t ->
  Fun.t -> bg * bg * bg

(** {3 Comparison} *)

(** [equal a b] returns [true] if bigraphs [a] and [b] are isomorphic, [false] otherwise. *)
val equal : bg -> bg -> bool

(** The type of bigraphs keys. *)			  
type bg_key = int

(** Compute the key of a bigraph. The key is similar to a hash. Note
    that different bigraphs can have the same key. *)
val key : bg -> bg_key

(** Same as {!Big.equal} but with fewer checks prior to the SAT solver
    invocation. This function is intended to be used after equality over keys
    has already failed. *)
val equal_opt : bg -> bg -> bool

(** {3 Matching} *)

(** The type of occurrences: an isomorphism over nodes, an isomorphism
    over edges and a function over hyper-edges. *)
type occ = Iso.t * Iso.t * Fun.t

(** [occurs t p] returns [true] if pattern [p] occurs in target [t], [false]
    otherwise. *)
val occurs : target:bg -> pattern:bg ->  bool

(** [occurrence t p trans] returns a pair of isomorphisms [(i,j)] if pattern [p]
    occurs in target [t]. Isos [i] and [j] are defined over nodes and edges,
    respectively. Argument [trans] is the transitive closure of the induced
    graph of [t].

    @raise NODE_FREE when [p] has an empty node set. *)
val occurrence : target:bg -> pattern:bg -> Place.bmatrix -> occ option

(*(** Same as {!Big.occurrence}.

    @raise NO_MATCH when there is no match.
    @raise NODE_FREE when [p] has an empty node set. *)				 
  val occurrence_exn : bg -> bg -> occ *)

(** [occurrences t p] returns a list of occurrences.

    @raise NODE_FREE when [p] has an empty node set. *)
val occurrences : target:bg -> pattern:bg -> occ list

(** [auto b] computes the non-trivial automorphisms of bigraph [b]. 

    @raise NODE_FREE when [p] has an empty node set. *)
val auto : bg -> (Iso.t * Iso.t) list

(** [rewrite o s r0 r1 eta] computes a bigraph obtained by replacing the
    occurrence of [r0] (specified by occurrence [o]) in [s] with [eta r1], where
    [eta] is a valid (no check performed) instantiation map.

    @raise Place.NOT_PRIME when [b] is not prime decomposable. *)
val rewrite : occ -> s:bg -> r0:bg -> r1:bg -> Fun.t option -> bg

(**/**)
