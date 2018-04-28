(** This module provides operations on bigraphical nodes.

    @author Michele Sevegnani *)

(** The type of a set of nodes. *)
type t

(** Add a node to the set. *)
val add : int -> Ctrl.t -> t -> t

(** Apply an isomorphism. *)
val apply : Iso.t -> t -> t

(** The empty node set. *)
val empty : t

(** Equality test. Node identities are ignored. *)
val equal : t -> t -> bool

(** [find_all c ns] finds all the nodes in node set [ns] with control contained
    by the sort that also contains control [c]. *)
val find_all : Ctrl.t -> t -> IntSet.t

(** Fold over a set. *)
val fold : (int -> Ctrl.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [get_ctrl i ns] returns the control of node [i] in node set [ns]. *)
val get_ctrl : int -> t -> Ctrl.t option

(** Return [true] if the set is empty. *)
val is_empty : t -> bool

(** Iterate over a set. *)
val iter : (int -> Ctrl.t -> unit) -> t -> unit

(** Compute the norm of a set. The norm is a defined as a sorted list of
    controls. Example:

    [["A"; "A(4)"; "C"; "D(3,6.2)"; "T"]]. *)
val norm : t -> string list

(** [not_sub a b] returns [true] when node set [a] is not a subset of node set
    [b]. *)
val not_sub : t -> t -> bool

(** Opposite of {!val:Nodes.to_string}.

    @raise Invalid_argument if the input cannot be parsed. *)
val of_string : string -> t

(** Return the cardinality of a node set. *)
val size : t -> int

(** [tens n0 n1] returns the disjoint union of name sets [n0] and [n1]. *)
val tens : t -> t -> t

(** Return a string representation of a node set. Example:

    ["\{(2, Ready:0),(0, A(1,4):1),(3, Fun:0),(1, Snd:2)\}"]. *)
val to_string : t -> string

(** Pretty printer. *)
val pp : Format.formatter -> t -> unit
  
(** [to_dot ns] returns a string expressing node shapes in dot format. *)
val to_dot: t -> string

(**/**)
