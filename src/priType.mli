module type P = sig
  type t
  val f_val : t -> bool
  val f_r_val : t -> bool
end

module Make (R : RrType.T)
    (P : P with type t = R.t list) : sig
  type p_class =
    | P_class of R.t list
    | P_rclass of R.t list
  val is_valid : p_class -> bool
  val is_valid_list : p_class list -> bool
  val is_reducible : p_class -> bool
  val cardinal : p_class list -> int
  val rewrite : Big.t -> p_class list -> Big.t * int
  val scan : Big.t * int ->
    part_f:  ((Big.t * R.label * R.t list) list ->
              ((int * (Big.t * R.label * R.t list)) list *
               (int * R.label * R.t list) list * int)) ->
    const_pri:p_class list -> p_class list ->
    ((int * (Big.t * R.label * R.t list)) list *
     (int * R.label * R.t list) list * int) * int
  val scan_sim : Big.t ->
    const_pri:p_class list -> p_class list ->
    (Big.t * R.label * R.t list) option * int
end
