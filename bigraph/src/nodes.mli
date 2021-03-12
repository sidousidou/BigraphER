(** This module provides operations on bigraphical nodes.

    @author Michele Sevegnani *)

type t
(** The type of a set of nodes. *)

val add : int -> Ctrl.t -> t -> t
(** Add a node to the set. *)

val apply : Iso.t -> t -> t
(** Apply an isomorphism. *)

val empty : t
(** The empty node set. *)

val equal : t -> t -> bool
(** Equality test. Node identities are ignored. *)

val find_all : Ctrl.t -> t -> IntSet.t
(** [find_all c ns] finds all the nodes in node set [ns] with control [c]. *)

val find_all_sort : Ctrl.t -> t -> IntSet.t
(** [find_all_sort c ns] finds all the nodes in node set [ns] with control
    contained by the sort that also contains control [c]. *)

val fold : (int -> Ctrl.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over a set. *)

val get_ctrl : int -> t -> Ctrl.t option
(** [get_ctrl i ns] returns the control of node [i] in node set [ns]. *)

val is_empty : t -> bool
(** Return [true] if the set is empty. *)

val iter : (int -> Ctrl.t -> unit) -> t -> unit
(** Iterate over a set. *)

val norm : t -> string list
(** Compute the norm of a set. The norm is a defined as a sorted list of
    controls. Example:

    {[ [ "A"; "A(4)"; "C"; "D(3,6.2)"; "T" ] ]} *)

val not_sub : t -> t -> bool
(** [not_sub a b] returns [true] when node set [a] is not a subset of node
    set [b]. *)

val of_string : string -> t
(** Opposite of {!val:Nodes.to_string}.

    @raise Invalid_argument if the input cannot be parsed. *)

val size : t -> int
(** Return the cardinality of a node set. *)

val tens : t -> t -> t
(** [tens n0 n1] returns the disjoint union of name sets [n0] and [n1]. *)

val to_string : t -> string
(** Return a string representation of a node set. Example:

    {[ {(2, Ready:0),(0, A(1,4):1),(3, Fun:0),(1, Snd:2)} ]} *)

val pp : Format.formatter -> t -> unit
(** Pretty printer. *)

val to_dot : t -> string
(** [to_dot ns] returns a string expressing node shapes in dot format. *)

(**/**)
