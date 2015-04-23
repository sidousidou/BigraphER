module type OccurType = sig
    type t (* (bg * float) or bg or (bg * int) *)
  end

module type T = sig

    (** The type of a rewrite rule. *)
    type t

    (** The type of an occurrence. *)
    type occ

    (** String representation of a rewrite rule. *)
    val to_string : t -> string

    (** Validity check. *)
    val is_valid : t -> bool

    (** [is_enabled b r] checks if rewrite rule [r] can be applied on bigraph
      [b]. *)
    val is_enabled : Big.bg -> t -> bool

    (** [fix b r_list] applies the rewrite rules in list [r_list] to bigraph [b]
      until a fixed point [b'] is reached. The result is fixed point [b'] and
      the number of rewriting steps performed. *)
    val fix : Big.bg -> t list -> Big.bg * int

    (** All the possible evolutions in one step. Total number of occurrences also
      returned. *)
    val step : Big.bg -> t list -> occ list * int

    (** Compute a random rewrite step. *)
    val random_step : Big.bg -> t list -> occ * int

  end

module Make (O : OccurType) : T with type t = O.t
