(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on bigraphs.
@author Michele Sevegnani *)

(** The type of bigraphs.*)
type bg = {
  p : Place.pg;  (** Place graph *)
  l : Link.Lg.t; (** Link graph *)
  n : Base.Nodes.t; (** Set of nodes *)
}

(** The type of interfaces.*)
type inter = Inter of int * Link.Face.t
(** [Inter (i,n)] creates an interface with ordinal [i] and names [n].*)

(** {6 Exceptions} *)

(** Raised when the composition in a sharing expression fails. *)
exception SHARING_ERROR

(** Raised when the arity of a control does not match the cardinality of a
   face. The first element is the arity while the second is the mismatching
   face. *)
exception CTRL_ERROR of int * Link.Face.t

(** Raised when an {!Iso.t} is not total. The first element is the cardinality
    of the domain while the second is the cardinality of the isomorphism's
    domain of definition. *)
exception ISO_ERROR of int * int

(** {6 Functions on interfaces} *)

(** [inter_equals a b] is [true] if [a] and [b] are equal.*)
val inter_equals : inter -> inter -> bool

(** [string_of_inter f] returns a string representation of interface [f]. *)
val string_of_inter: inter -> string

(** Returns the ordinal of an interface. *)
val ord_of_inter : inter -> int

(** Returns the face of an interface. *)
val face_of_inter : inter -> Link.Face.t

(** {6 Funtions on bigraphs}*)

(** [string_of_bg b] returns a string representation of bigraph [b]. *)
val string_of_bg: bg -> string

(** [get_dot b i] returns a string expressing bigraph [b] named [i] in 
    dot format. *)
val get_dot : bg -> string -> string

(** [inner b] returns the inner face of bigraph [b].*)
val inner : bg -> inter

(** [outer b] returns the outer face of bigraph [b].*)
val outer : bg -> inter

(** [apply_iso i b] applies isomorphism [i] to bigraph [b].
    @raise ISO_ERROR when [i] is not total over the nodes of [b].*)
val apply_iso : Base.Iso.t -> bg -> bg

(** [placing l r f] builds a placing with [r] roots by parsing list [l]. The
    format of [l] is the same as the input for {!Place.parse_placing}.
    The link graph is the idendity over face [f].*)
val placing : int list list -> int -> Link.Face.t -> bg

(** {6 Elementary bigraphs graphs} *)

(** [id i] returns an identity over interface [i].*)
val id : inter -> bg

(** [id_eps] returns an empty identity.*)
val id_eps : bg

(**[merge n] returns a bigraph consisting of one root and [n] sites.*)
val merge : int -> bg

(** [split n] returns a bigraph consisting of one site and [n] roots.*)
val split : int -> bg

(** [one] returns elementary bigraph consisting of one root.*)
val one : bg

(** [zero] returns elementary bigraphs consisting of one site.*)
val zero : bg

(** [sym i j] returns a symmetry on interfaces [i] and [j].*)
val sym : inter -> inter -> bg

(** [ion ns c] returns an ion of control [c]. It's outer names are [ns].
    @raise CONTROL_ERROR when [ns] has size different than the arity of [c].*)
val ion : Link.Face.t -> Base.ctrl -> bg

(** [sub n m] returns a substitution where [n] and [m] are the inner and outer 
    faces, respectively.*)
val sub : Link.Face.t -> Link.Face.t -> bg

(** [closure f] returns a closure of interface [f].*)
val closure : Link.Face.t -> bg

(** [intro f] returns a fresh set of names [f].*)
val intro : Link.Face.t -> bg

(** {6 Operations} *)

(** [tens a b] computes the tensor product of bigraphs [a] and [b].
    @raise Link.NAMES_ALREADY_DEFINED when [a] and [b] have shared names.*)
val tens : bg -> bg -> bg

(** [comp a b] computes the composition of bigraphs [a] and [b].
    @raise Link.FACES_MISMATCH when the names in the mediating interface
     differ.
    @raise Place.COMP_ERROR when the number of sites and roots of [a] and [b],
     respectively do not match. *)
val comp : bg -> bg -> bg

(** [ppar a b] computes the parallel product of bigraphs [a] and [b].*)
val ppar : bg -> bg -> bg

(** [ppar_of_list bs] computes the parallel product of all the bigraphs in list
    [bs].*)
val ppar_of_list: bg list -> bg

(** [par a b] computes the merge product of bigraphs [a] and [b].*)
val par : bg -> bg -> bg

(** [par_of_list bs] computes the merge product of all the bigraphs in list
    [bs].*)
val par_of_list: bg list -> bg

(** [nest a b] computes the bigraph resulting from nesting bigraph [b] in 
    bigraph [a]. Common names are shared. *)
val nest : bg -> bg -> bg

(** [share f psi g] computes the bigraphs obtained by sharing bigraph [f] in
    bigraph [g] by using placing [psi].
    @raise SHARING_ERROR if [psi] is not a placing. *)
val share : bg -> bg -> bg -> bg

(** {6 Predicates} *)

(** [is_id b] returns [true] if bigraph [b] is an identity, [false] otherwise.*)
val is_id : bg -> bool

(** [is_plc b] returns [true] if bigraph [b] is a placing, [false] otherwise.*)
val is_plc : bg -> bool

(** [is_wir b] returns [true] if bigraph [b] is a wiring, [false] otherwise.*)
val is_wir : bg -> bool

(** [is_epi b] returns [true] if bigraph [b] is epimorphic, [false] otherwise.*)
val is_epi : bg -> bool

(** [is_mono b] returns [true] if bigraph [b] is monomorphic, [false] 
    otherwise.*)
val is_mono : bg -> bool

(** [is_guard b] returns [true] if bigraph [b] is an guarded, [false] 
    otherwise.*)
val is_guard : bg -> bool

(** [is_solid b] returns [true] if bigraph [b] is solid, [false] otherwise.*)
val is_solid : bg -> bool

(** {6 Decompositions} *)

(** [decomp t p i_v i_e] builds the decomposition of target [t] given pattern
    [p], node isomorphism [i_v] and edge isomorphism [i_e]. The isomorphism
    are from [p] to [t]. The elements in the result are the context, the 
    parameter and the identity of the decomposition. *)
val decomp :  bg -> bg -> Base.Iso.t -> Base.Iso.t -> bg * bg * bg

(** [levels b] computes the decomposition in levels of [b]. *)
val levels : bg -> bg list

(*val equals : bg -> bg -> out_channel -> in_channel -> bool*)
(*val norm*)

(* Matching string*)
(*val match_string : bg -> string

val occurrences : bg -> bg -> out_channel -> in_channel -> (Base.Iso.t * Base.Iso.t) list

val occurrence : bg -> bg -> out_channel -> in_channel -> Base.Iso.t * Base.Iso.t

val occurs : bg -> bg -> out_channel -> in_channel -> bool*)

