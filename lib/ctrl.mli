(** The module of bigraphical controls.
    @author Michele Sevegnani *)

(** [C (s, ar)] creates a control of arity [ar] named [s]. *)
type t = C of string * int

(** [to_string c] gives the string representation of control [c] in the form
    [name:arity]. *)
val to_string : t -> string

(** [arity c] returns the arity of control [c]. *)
val arity : t -> int

(** [name c] returns the arity of control [c]. *)		   
val name : t -> string		   

(** Equality for type {!Ctrl.t}. *)
val (=) : t -> t -> bool

(** Comparison function. *)
val compare : t -> t -> int			   

(**/**)
