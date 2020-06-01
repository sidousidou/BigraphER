(** Input signature of the functor {!Priority.Make}. *)
module type Val_Check = sig
  type t
  (** The type of a priority class. *)

  val f_val : t -> bool
  (** Predicate to check the validity of a priority class. *)

  val f_r_val : t -> bool
  (** Predicate to check the validity of a reducible priority class. *)
end

(** Output signature fo the functor {!Priority.Make}. *)
module type P = sig
  type r_t

  type r_label

  type p_class = P_class of r_t list | P_rclass of r_t list

  val is_valid : p_class -> bool

  val is_valid_list : p_class list -> bool

  val cardinal : p_class list -> int

  val rewrite : Big.t -> p_class list -> Big.t * int

  val scan :
    Big.t * int ->
    part_f:
      ((Big.t * r_label * r_t list) list ->
      (int * (Big.t * r_label * r_t list)) list
      * (int * r_label * r_t list) list
      * int) ->
    const_pri:p_class list ->
    p_class list ->
    ( (int * (Big.t * r_label * r_t list)) list
    * (int * r_label * r_t list) list
    * int )
    * int
  (** Iterate over priority classes. *)

  val scan_sim :
    Big.t ->
    const_pri:p_class list ->
    p_class list ->
    (Big.t * r_label * r_t list) option * int
end

(** Functor building a concrete implementation of priority classes. *)
module Make
    (S : Solver.M)
    (R : RrType.T)
    (V : Val_Check with type t := R.t list) :
  P with type r_t := R.t and type r_label := R.label

(**/**)
