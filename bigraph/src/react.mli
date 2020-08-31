(** Input signature of the functor {!React.Make}. *)
module type R = sig
  type t

  type ac

  type label

  val name : t -> string

  val lhs : t -> Big.t

  val rhs : t -> Big.t

  val conds : t -> ac list

  val l : t -> label

  val equal : t -> t -> bool

  val map : t -> Fun.t option

  val merge_occ :
    Big.t * label * t list ->
    Big.t * label * t list ->
    Big.t * label * t list

  val val_chk : t -> bool

  val val_chk_error_msg : string

  val string_of_label : label -> string

  val make :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    t

  val step_post :
    (Big.t * label * t list) list * int ->
    (Big.t * label * t list) list * int
  (** Postprocessing rewrite step. *)

  val random_step_post :
    (Big.t * label * t list) list * int ->
    (Big.t * label * t list) option * int
  (** Postprocessing random rewrite step. *)
end

(** Output signature of the functor {!React.Make}. *)
module type T = sig
  type t
  (** The type of reaction rules. *)

  type ac
  (** The type of application conditions. *)

  type label
  (** The type of reaction rule lables. *)

  type react_error

  exception NOT_VALID of react_error

  val name : t -> string
  (** Returns the name of a rewrite rule. *)

  val lhs : t -> Big.t
  (** Return the left-hand side of a rewrite rule. *)

  val rhs : t -> Big.t
  (** Return the right-hand side of a rewrite rule. *)

  val conds : t -> ac list
  (** Return application conditions for a rewrite rule. *)

  val l : t -> label
  (** Return the label of a rewrite rule. *)

  val equal : t -> t -> bool
  (** Equality for reaction rules *)

  val map : t -> Fun.t option
  (** Return the instantition map of a rewrite rule. *)

  val make :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    t
  (** Create a new reaction rule. *)

  val to_string : t -> string
  (** String representation of a rewrite rule. *)

  val is_valid : t -> bool
  (** Validity check. *)

  val is_valid_exn : t -> bool
  (** Same as {!is_valid} but an exception is raised instead of returning
      [false].

      @raise NOT_VALID when the reaction rule is not valid. *)

  val string_of_react_err : react_error -> string
  (** Convert to string the output of the validity check. *)

  val is_enabled : Big.t -> t -> bool
  (** [is_enabled b r] checks if rewrite rule [r] can be applied to bigraph
      [b]. *)

  val filter_iso :
    (Big.t * label * t list) list -> (Big.t * label * t list) list
  (** Merge isomorphic occurrences *)

  val apply : Big.t -> t list -> Big.t option
  (** Apply a list of reaction rules in sequence. Non-enabled rules are
      ignored. *)

  val fix : Big.t -> t list -> Big.t * int
  (** [fix b r_list] applies the rewrite rules in list [r_list] to bigraph
      [b] until a fixed point [b'] is reached. The result is fixed point [b']
      and the number of rewriting steps performed. Note, [b] is returned when
      no rewriting is performed. *)

  val step : Big.t -> t list -> (Big.t * label * t list) list * int
  (** All the possible evolutions in one step. Total number of occurrences
      also returned. *)

  val random_step : Big.t -> t list -> (Big.t * label * t list) option * int
  (** Random step. *)

  (** Memoised interface *)
  module Memo : sig
    val is_enabled : Big.t -> Sparse.t -> t -> bool

    val fix : Big.t -> Sparse.t -> t list -> Big.t * int

    val step :
      Big.t ->
      Sparse.t ->
      (t * (Iso.t * Iso.t) list) list ->
      (Big.t * label * t list) list * int

    val random_step :
      Big.t ->
      Sparse.t ->
      (t * (Iso.t * Iso.t) list) list ->
      (Big.t * label * t list) option * int
  end
end

(** Functor building a concrete implementation of basic operations on rewrite
    rules. *)
module Make (S : Solver.M) (AC : AppCond.C) (R : R with type ac = AppCond.t) :
  T with type t = R.t and type label = R.label and type ac = R.ac

(**/**)
