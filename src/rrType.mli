(** Type-dependent interface of rewrite rules. *)
module type R =
sig
  type t
  type label
  val name : t -> string
  val action : t -> string
  val lhs : t -> Big.t
  val rhs : t -> Big.t
  val l : t -> label
  val equal : t -> t -> bool
  val map : t -> Fun.t option
  val merge_occ : (Big.t * label * t list) -> (Big.t * label * t list) ->
    (Big.t * label * t list)
  val val_chk : t -> bool
  val val_chk_error_msg : string
  val string_of_label : label -> string
  val parse : name:string -> lhs:Big.t -> rhs:Big.t ->
    label -> Fun.t option -> t
  val step : Big.t -> t list -> (Big.t * label * t list) list * int
  val random_step : Big.t -> t list -> (Big.t * label * t list) option * int
end

module type T =
sig
  type t
  type label
  type react_error
  exception NOT_VALID of react_error
  val name : t -> string
  val action : t -> string
  val lhs : t -> Big.t
  val rhs : t -> Big.t
  val l : t -> label
  val equal : t -> t -> bool
  val map : t -> Fun.t option
  val merge_occ : (Big.t * label * t list) -> (Big.t * label * t list)
    -> (Big.t * label * t list)
  val parse : name:string -> lhs:Big.t -> rhs:Big.t ->
    label -> Fun.t option -> t
  val to_string : t -> string
  val is_valid : t -> bool
  val is_valid_exn : t -> bool
  val string_of_react_err : react_error -> string
  val is_enabled : Big.t -> t -> bool
  val apply : Big.t -> t list -> Big.t option
  val fix : Big.t -> t list -> Big.t * int
  val step : Big.t -> t list -> (Big.t * label * t list) list * int
  val random_step : Big.t -> t list -> (Big.t * label * t list) option * int
end

(** Module for the concrete implementation of basic operations on rewrite
    rules. *)
module Make (R : R) : sig

  type t = R.t

  type label = R.label

  type react_error

  exception NOT_VALID of react_error

  (** Returns the name of a rewrite rule. *)
  val name : t -> string

  (** Returns the MDP action of a rewrite rule. *)
  val action : t -> string

  (** Return the left-hand side of a rewrite rule. *)
  val lhs : t -> Big.t

  (** Return the right-hand side of a rewrite rule. *)
  val rhs : t -> Big.t

  (** Return the label of a rewrite rule. *)
  val l : t -> label

  (** Eqaulity for reaction rules *)
  val equal : t -> t -> bool

  (** Return the instantition map of a rewrite rule. *)
  val map : t -> Fun.t option

  (** Merge two occurrences. *)
  val merge_occ : (Big.t * label * t list) -> (Big.t * label * t list) ->
    (Big.t * label * t list)

  (** Creare a new reaction rule. *)
  val parse : name:string -> lhs:Big.t -> rhs:Big.t ->
    label -> Fun.t option -> t

  (** String representation of a rewrite rule. *)
  val to_string : t -> string

  (** Validity check. *)
  val is_valid : t -> bool

  (** Same as {!is_valid} but an exception is raised instead of returning
      [false].

      @raise NOT_VALID when the reaction rule is not valid. *)
  val is_valid_exn : t -> bool

  (** Convert to string the output of the validity check. *)
  val string_of_react_err : react_error -> string

  (** [is_enabled b r] checks if rewrite rule [r] can be applied to bigraph
      [b]. *)
  val is_enabled : Big.t -> t -> bool

  (** Apply a list of reaction rules in sequence. Non-enabled rules are
      ignored. *)
  val apply : Big.t -> t list -> Big.t option

  (** [fix b r_list] applies the rewrite rules in list [r_list] to bigraph [b]
      until a fixed point [b'] is reached. The result is fixed point [b'] and
      the number of rewriting steps performed. Note, [b] is returned when no
      rewriting is performed. *)
  val fix : Big.t -> t list -> Big.t * int

  (** All the possible evolutions in one step. Total number of occurrences also
      returned. *)
  val step : Big.t -> t list -> (Big.t * label * t list) list * int

  (** Random step. *)
  val random_step : Big.t -> t list -> (Big.t * label * t list) option * int

end

(** Merge isomorphic occurrences *)
val filter_iso :
  (Big.t * 'a * 'b list -> Big.t * 'a * 'b list -> Big.t * 'a * 'b list) ->
  (Big.t * 'a * 'b list) list -> (Big.t * 'a * 'b list) list

(** Generic step function *)
val gen_step :
  Big.t ->
  'a list ->
  ((Big.t * 'b * 'a list) -> (Big.t * 'b * 'a list) ->
   (Big.t * 'b * 'a list)) ->
  lhs:('a -> Big.t) ->
  rhs:('a -> Big.t) ->
  label:('a -> 'b) ->
  map:('a -> Fun.t option) ->
  (Big.t * 'b * 'a list) list * int

(**/**)
