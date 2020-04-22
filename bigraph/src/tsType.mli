(* Labelled directed graphs *)
module type G = sig
  type t

  type l

  val init : int -> String.t list -> t

  val states : t -> (int * Big.t) Base.H_int.t

  val label : t -> Base.S_string.t * int Base.H_string.t

  val edges : t -> (int * l * string) Base.H_int.t

  val string_of_l : l -> string
end

(* Type of computation limit *)
module type L = sig
  type t

  type l

  val init : t

  val increment : t -> l -> t

  val is_greater : t -> t -> bool

  val to_string : t -> string
end

(* Reactive system descriptions *)
module type T = sig
  val typ : Rs.t
end

(* Reactive systems *)
module type RS = sig
  type react

  type p_class = P_class of react list | P_rclass of react list

  type graph

  type label

  type limit

  type react_error

  val typ : Rs.t

  val string_of_react : react -> string

  val name : react -> string

  val lhs : react -> Big.t

  val rhs : react -> Big.t

  val conds : react -> AppCond.t list

  val map : react -> Fun.t option

  val string_of_limit : limit -> string

  val is_valid_react : react -> bool

  exception NOT_VALID of react_error

  val is_valid_react_exn : react -> bool

  val string_of_react_err : react_error -> string

  val equal_react : react -> react -> bool

  val is_valid_priority : p_class -> bool

  val is_valid_priority_list : p_class list -> bool

  val cardinal : p_class list -> int

  val step : Big.t -> react list -> (Big.t * label * react list) list * int

  val random_step :
    Big.t -> react list -> (Big.t * label * react list) option * int

  val apply : Big.t -> react list -> Big.t option

  val fix : Big.t -> react list -> Big.t * int

  val rewrite : Big.t -> p_class list -> Big.t * int

  exception MAX of graph * Stats.t

  val bfs :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.t) list ->
    max:int ->
    iter_f:(int -> Big.t -> unit) ->
    graph * Stats.t

  exception DEADLOCK of graph * Stats.t * limit

  exception LIMIT of graph * Stats.t

  val sim :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.t) list ->
    init_size:int ->
    stop:limit ->
    iter_f:(int -> Big.t -> unit) ->
    graph * Stats.t

  val to_prism : graph -> string

  val to_dot : graph -> path:string -> name:string -> string

  val to_lab : graph -> string

  val iter_states : (int -> Big.t -> unit) -> graph -> unit

  val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a

  val iter_edges : (int -> int -> label -> unit) -> graph -> unit

  val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a

  val parse_react_unsafe :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:AppCond.t list ->
    label ->
    Fun.t option ->
    react

  val parse_react :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:AppCond.t list ->
    label ->
    Fun.t option ->
    react option
end

module Make
    (R : RrType.T) (P : sig
      type p_class = P_class of R.t list | P_rclass of R.t list

      val is_valid : p_class -> bool

      val is_valid_list : p_class list -> bool

      val cardinal : p_class list -> int

      val rewrite : Big.t -> p_class list -> Big.t * int

      val scan :
        Big.t * int ->
        part_f:
          ((Big.t * R.label * R.t list) list ->
          (int * (Big.t * R.label * R.t list)) list
          * (int * R.label * R.t list) list
          * int) ->
        const_pri:p_class list ->
        p_class list ->
        ( (int * (Big.t * R.label * R.t list)) list
        * (int * R.label * R.t list) list
        * int )
        * int

      val scan_sim :
        Big.t ->
        const_pri:p_class list ->
        p_class list ->
        (Big.t * R.label * R.t list) option * int
    end)
    (L : L with type l = R.label)
    (G : G with type l = R.label)
    (Ty : T) : sig
  type t = G.t

  type p_class = P.p_class = P_class of R.t list | P_rclass of R.t list

  type limit = L.t

  type label = R.label

  type react_error = R.react_error

  exception MAX of t * Stats.t

  exception LIMIT of t * Stats.t

  exception DEADLOCK of t * Stats.t * limit

  exception NOT_VALID of react_error

  val typ : Rs.t

  val string_of_react : R.t -> string

  val parse_react_unsafe :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:AppCond.t list ->
    label ->
    Fun.t option ->
    R.t

  val parse_react :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:AppCond.t list ->
    label ->
    Fun.t option ->
    R.t option

  val name : R.t -> string

  val lhs : R.t -> Big.t

  val rhs : R.t -> Big.t

  val conds : R.t -> AppCond.t list

  val map : R.t -> Fun.t option

  val string_of_limit : limit -> string

  val is_valid_react : R.t -> bool

  val is_valid_react_exn : R.t -> bool

  val equal_react : R.t -> R.t -> bool

  val string_of_react_err : react_error -> string

  val fix : Big.t -> R.t list -> Big.t * int

  val step : Big.t -> R.t list -> (Big.t * R.label * R.t list) list * int

  val random_step :
    Big.t -> R.t list -> (Big.t * R.label * R.t list) option * int

  val apply : Big.t -> R.t list -> Big.t option

  val is_valid_priority : p_class -> bool

  val is_valid_priority_list : p_class list -> bool

  val cardinal : p_class list -> int

  val rewrite : Big.t -> p_class list -> Big.t * int

  val bfs :
    s0:Big.t ->
    priorities:P.p_class list ->
    predicates:(string * Big.t) list ->
    max:int ->
    iter_f:(int -> Big.t -> unit) ->
    t * Stats.t

  val sim :
    s0:Big.t ->
    priorities:P.p_class list ->
    predicates:(string * Big.t) list ->
    init_size:int ->
    stop:limit ->
    iter_f:(int -> Big.t -> unit) ->
    t * Stats.t

  val to_prism : t -> string

  val to_dot : t -> path:string -> name:string -> string

  val to_lab : t -> string

  val iter_states : (int -> Big.t -> unit) -> t -> unit

  val fold_states : (int -> Big.t -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_edges : (int -> int -> label -> unit) -> t -> unit

  val fold_edges : (int -> int -> label -> 'a -> 'a) -> t -> 'a -> 'a
end
