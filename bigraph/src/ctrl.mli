(** The module of bigraphical controls.
    @author Michele Sevegnani *)

(** The type of control parameters. *)
type param =
  | I of int       (** Integer parameter *)
  | F of float     (** Float parameter   *)
  | S of String.t  (** String parameter  *)

(** [C (s, ps, ar)] creates a control of arity [ar] named [s] with parameters
    [ps]. *)
type t = C of String.t * param list * int

(** [arity c] returns the arity of control [c]. *)
val arity : t -> int

(** Comparison function. Equal control names imply equal arities and parameter
    types. {b Note, no check is performed.} *)
val compare : t -> t -> int

(** Equality for type {!Ctrl.t}. *)
val equal : t -> t -> bool

(** Return a string containing the name of a control and the values of its
    parameters. For example, [long_name (C (S,[I 3;F 4.6],3))] produces the
    following string:

    ["S(3,4.6)"] *)
val long_name : t -> string

(** [name c] returns the name of control [c]. *)
val name : t -> string

(** Opposite of {!val:Ctrl.to_string}.

    @raise Invalid_argument if the input cannot be parsed. *)
val of_string : string -> t

(** [parse_name s ar] returns a control of arity [ar] obtained by parsing string
    [s]. *)
val parse_name : string -> int -> t

(** [to_string c] gives the string representation of control [c] in the form
    ["name:arity"] or ["name(par_0,par_1,...,par_n):arity"]. *)
val to_string : t -> string

(**/**)
