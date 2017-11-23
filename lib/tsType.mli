module type G = sig
  type t
  type edge_type
  val init : int -> String.t list -> t
  val states : t -> (int * Big.bg) Base.H_int.t
  val label : t -> (Base.S_string.t * int Base.H_string.t)
  val edges : t -> edge_type Base.H_int.t
  val dest : edge_type -> int
  val string_of_arrow : edge_type -> string
end

module type L = sig
  type t
  type occ
  val init : t
  val increment : t -> occ -> t
  (* is_greater a b = a > b *)
  val is_greater : t -> t -> bool
  val to_string : t -> string
end

module type T = sig
  val typ : Rs.t
end

(* The core interface of a RS *)
module type RS_core = sig
  type react
  type p_class =
    | P_class of react list
    | P_rclass of react list
  type graph
  type react_error
  type occ
  type limit
  val typ : Rs.t
  val string_of_react : react -> string
  val lhs_of_react : react -> Big.bg
  val rhs_of_react : react -> Big.bg
  val string_of_limit : limit -> string
  val is_valid_react : react -> bool
  exception NOT_VALID of react_error
  val is_valid_react_exn : react -> bool
  val string_of_react_err : react_error -> string
  val is_valid_priority : p_class -> bool
  val is_valid_priority_list : p_class list -> bool
  val cardinal : p_class list -> int
  val step : Big.bg -> react list -> occ list * int
  val random_step : Big.bg -> react list -> occ option * int
  val apply : Big.bg -> react list -> Big.bg option
  val fix : Big.bg -> react list -> Big.bg * int
  val rewrite : Big.bg -> p_class list -> Big.bg * int  
  exception MAX of graph * Stats.t
  val bfs :
    s0:Big.bg ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.bg) list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> graph * Stats.t
  exception DEADLOCK of graph * Stats.t * limit
  exception LIMIT of graph * Stats.t  
  val sim :
    s0:Big.bg ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.bg) list ->
    init_size:int -> stop:limit -> iter_f:(int -> Big.bg -> unit) -> graph * Stats.t
  val to_prism : graph -> string
  val to_dot : graph -> name:string -> string
  val to_lab : graph -> string
  val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit
  val write_svg : graph -> name:string -> path:string -> int
  val write_prism : graph -> name:string -> path:string -> int
  val write_lab : graph -> name:string -> path:string -> int
  val write_dot : graph -> name:string -> path:string -> int
  val write_json : graph -> name:string -> path:string -> int
end

(* The complete interface of a Reactive System *)
module type RS = sig
  include RS_core
  type label
  val parse_react_unsafe : lhs:Big.bg -> rhs:Big.bg -> label -> Fun.t option -> react
  val parse_react : lhs:Big.bg -> rhs:Big.bg -> label -> Fun.t option -> react option
end

(* Discrete time or continuous time *)
module type TT = sig
  type t
  val stop : t
end

module Make (R : RrType.T)
    (P : sig
       type p_class =
         | P_class of R.t list
         | P_rclass of R.t list
       val is_valid : p_class -> bool
       val is_valid_list : p_class list -> bool
       val cardinal : p_class list -> int
       val rewrite : Big.bg -> p_class list -> Big.bg * int
       val scan : Big.bg * int ->
         part_f:(R.occ list ->
                 ((int * R.occ) list * R.edge list * int)) ->
         const_pri:p_class list -> p_class list ->
         ((int * R.occ) list * R.edge list * int) * int
       val scan_sim : Big.bg ->
         const_pri:p_class list -> p_class list ->
         R.occ option * int
     end)
    (L : L with type occ = R.occ)
    (G : G with type edge_type = R.edge)
    (Ty : T): sig

  type t = G.t

  type p_class = P.p_class =
    | P_class of R.t list
    | P_rclass of R.t list

  type occ = R.occ
  
  type limit = L.t

  type label = R.label
                 
  type react_error = R.react_error

  exception MAX of t * Stats.t

  exception LIMIT of t * Stats.t

  exception DEADLOCK of t * Stats.t * limit

  exception NOT_VALID of react_error

  val typ : Rs.t
  
  val string_of_react : R.t -> string

  val parse_react_unsafe : lhs:Big.bg -> rhs:Big.bg -> label -> Fun.t option -> R.t

  val parse_react : lhs:Big.bg -> rhs:Big.bg -> label -> Fun.t option -> R.t option
  
  val lhs_of_react : R.t -> Big.bg

  val rhs_of_react : R.t -> Big.bg
  
  val string_of_limit : limit -> string
  
  val is_valid_react : R.t -> bool

  val is_valid_react_exn : R.t -> bool

  val string_of_react_err : react_error -> string

  val fix : Big.bg -> R.t list -> Big.bg * int

  val step : Big.bg -> R.t list -> R.occ list * int

  val random_step : Big.bg -> R.t list -> R.occ option * int

  val apply : Big.bg -> R.t list -> Big.bg option
                                    
  val is_valid_priority : p_class -> bool

  val is_valid_priority_list : p_class list -> bool

  val cardinal : p_class list -> int

  val rewrite : Big.bg -> p_class list -> Big.bg * int

  val bfs :
    s0:Big.bg ->
    priorities:P.p_class list ->
    predicates:(string * Big.bg) list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> t * Stats.t

  val sim :
    s0:Big.bg ->
    priorities:P.p_class list ->
    predicates:(string * Big.bg) list ->
    init_size:int ->
    stop:limit -> iter_f:(int -> Big.bg -> unit) -> t * Stats.t

  val to_prism : t -> string

  val to_dot : t -> name: string -> string

  val to_lab : t -> string

  val iter_states : f:(int -> Big.bg -> unit) -> t -> unit

  val write_svg : t -> name:string -> path:string -> int

  val write_prism : t -> name:string -> path:string -> int

  val write_lab : t -> name:string -> path:string -> int

  val write_dot : t -> name:string -> path:string -> int
    
  val write_json : t -> name:string -> path:string -> int

end
