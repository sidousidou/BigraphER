(** This module provides operations on bigraphical nodes.
    @author Michele Sevegnani *)

(** The type of a set of nodes. *)		
type t =
  { ctrl : Ctrl.t Base.M_int.t;      (** Map between node identifiers and controls. *)
    sort : IntSet.t Base.M_string.t; (** Map between controls and node identifiers. *)
    size : int                       (** Cardinality of the set. *)
  }

(** Add a node to the set. *)
val add : int -> Ctrl.t -> t -> t

(** Fold over a set. *)				    
val fold : (int -> Ctrl.t -> 'a -> 'a) -> t -> 'a -> 'a

(** The empty node set. *)
val empty : t

(** Return [true] if the set is empty. *)			
val is_empty : t -> bool

(** Return a string representation of a node set. Example: 

    [\{(2, Ready:0),(0, A:1),(3, Fun:0),(1, Snd:2)\}]. *)
val to_string : t -> string

(** Return a JSON representation of a node set. Example:
 
    ["nodes": [
       \{
          "node_id": 1,
          "control": \{
            "control_id": "Send",
            "control_arity": 2
          \}
       \},
       \{
          "node_id": 2,
          "control": \{
            "control_id": "New",
            "control_arity": 1
          \}
       \}
     ]]*)
val json_of_nodes : t -> string
                           
val json_of_nodes_f : t -> Base.JSON.json_node

(** Return a string representation of the sorts of a node set. Example:

    [\{Ready, A, Fun, Snd\}]. *)
val string_of_sorts : t -> string

(** [to_dot ns] returns a string expressing node shapes in dot format. *)
val to_dot: t -> string

(** [get_ctrl_exn i ns] returns the control of node [i] in node set [ns].  

    @raise Not_found if the node identifier is not present in the set. *)
val get_ctrl_exn : int -> t -> Ctrl.t 

(** [find_all ns c] returns the set of nodes of control [c] in node set [ns]. *)
val find_all : Ctrl.t -> t -> IntSet.t 

(** [tens n0 n1] returns the disjoint union of name sets [n0] and [n1]. *)
val tens : t -> t -> t

(** [parse s m] parses the string representation of a node set. Argument [m]
      is a map between node identifiers and arities. An example for [s] is:
    ["A A B D"]. *)
val parse : string -> int Base.M_int.t -> t

(** Apply an isomorphism.

    @raise Not_found if a node identifier is not in the domain of the
    isomorphism.  *)
val apply_exn : t -> int Iso.t -> t

(** Apply an isomorphism only to nodes in the domain of the isomorphism.  Other
    nodes are discarded. *)
val filter_apply_iso : t -> int Iso.t -> t

(** [not_sub a b] returns [true] when node set [a] is not a subset of node set
    [b]. *)
val not_sub : t -> t -> bool

(** Compute the norm of a set. The norm is a defined as a sorted list of
    controls. Example:

    [["A"; "A"; "C"; "D"; "T"]]. *)
val norm : t -> string list

(** Equality test. Node identities are ignored. *)		    
val equal : t -> t -> bool

(**/**)
