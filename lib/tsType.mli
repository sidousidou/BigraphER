module type G = sig
  type t
  type edge_type
  val init : int -> t
  val states : t -> (int * Big.bg) Base.H_int.t
  val label : t -> int Base.H_string.t
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

type stats_t = { time : float; 
                 states : int;  
                 trans : int;  
                 occs : int }
               
module type S = sig
  type t = stats_t
  type g
  val init : float -> g -> int -> t
  val to_string : t -> (string * string * bool) list
end

(* The interface of a Reactive System *)
module type RS = sig
  type react
  type p_class =
    | P_class of react list
    | P_rclass of react list
  type stats = stats_t
  type graph
  type react_error
  type occ
  type limit
  type label
  val typ : Rs.t
  val string_of_stats : stats -> (string * string * bool) list
  val string_of_react : react -> string
  val parse_react : lhs:Big.bg -> rhs:Big.bg -> label option -> int Fun.t option -> react
  val lhs_of_react : react -> Big.bg
  val rhs_of_react : react -> Big.bg
  val string_of_limit : limit -> string
  val is_valid_react : react -> bool
  exception NOT_VALID of react_error
  val is_valid_react_exn : react -> bool
  val string_of_react_err : react_error -> string
  (*  val is_inst : react -> bool *)
  val is_valid_priority : p_class -> bool
  val is_valid_priority_list : p_class list -> bool
  val cardinal : p_class list -> int
  val step : Big.bg -> react list -> occ list * int
  val random_step : Big.bg -> react list -> occ option * int
  val fix : Big.bg -> react list -> Big.bg * int
  val rewrite : Big.bg -> p_class list -> Big.bg * int  
  exception MAX of graph * stats
  val bfs :
    s0:Big.bg ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.bg) list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> graph * stats
  exception DEADLOCK of graph * stats * limit
  exception LIMIT of graph * stats  
  val sim :
    s0:Big.bg ->
    priorities:p_class list ->
    predicates:(Base.H_string.key * Big.bg) list ->
    init_size:int -> stop:limit -> iter_f:(int -> Big.bg -> unit) -> graph * stats
  val to_prism : graph -> string
  val to_dot : graph -> name:string -> string
  val to_lab : graph -> string
  val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit
  val write_svg : graph -> name:string -> path:string -> int
  val write_prism : graph -> name:string -> path:string -> int
  val write_lab : graph -> name:string -> path:string -> int
  val write_dot : graph -> name:string -> path:string -> int
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

  type stats = stats_t
  
  type occ = R.occ
  
  type limit = L.t

  type label = R.label
                 
  type react_error = R.react_error

  exception MAX of t * stats

  exception LIMIT of t * stats

  exception DEADLOCK of t * stats * limit

  exception NOT_VALID of react_error

  val typ : Rs.t

  val string_of_stats : stats -> (string * string * bool) list

  val string_of_react : R.t -> string
    
  val parse_react : lhs:Big.bg -> rhs:Big.bg -> label option -> int Fun.t option -> R.t

  val lhs_of_react : R.t -> Big.bg

  val rhs_of_react : R.t -> Big.bg
  
  val string_of_limit : limit -> string
  
  val is_valid_react : R.t -> bool

  val is_valid_react_exn : R.t -> bool

  val string_of_react_err : react_error -> string

  val fix : Big.bg -> R.t list -> Big.bg * int

  val step : Big.bg -> R.t list -> R.occ list * int

  val random_step : Big.bg -> R.t list -> R.occ option * int
                                                                   
  val is_valid_priority : p_class -> bool

  val is_valid_priority_list : p_class list -> bool

  val cardinal : p_class list -> int

  val rewrite : Big.bg -> p_class list -> Big.bg * int

  val bfs :
    s0:Big.bg ->
    priorities:P.p_class list ->
    predicates:(string * Big.bg) list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> t * stats

  val sim :
    s0:Big.bg ->
    priorities:P.p_class list ->
    predicates:(string * Big.bg) list ->
    init_size:int ->
    stop:limit -> iter_f:(int -> Big.bg -> unit) -> t * stats

  val to_prism : t -> string

  val to_dot : t -> name: string -> string

  val to_lab : t -> string

  val iter_states : f:(int -> Big.bg -> unit) -> t -> unit

  val write_svg : t -> name:string -> path:string -> int

  val write_prism : t -> name:string -> path:string -> int

  val write_lab : t -> name:string -> path:string -> int

  val write_dot : t -> name:string -> path:string -> int

end
