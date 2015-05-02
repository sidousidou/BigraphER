module type G =
  sig
    type t
    type edge_type
    val init : int -> t
    val states : t -> (Big.bg_key, int * Big.bg) Hashtbl.t
    val label : t -> (int, int) Hashtbl.t
    val edges : t -> (int, edge_type) Hashtbl.t
    val dest : edge_type -> int
    val string_of_arrow : edge_type -> string
  end

module type L = sig type t val is_greater : t -> t -> bool end

module type S =
  sig
    type t
    val init : t0:float -> t
    val update : time:float -> states:int -> reacts:int -> occs:int ->
		 old_stats:t -> t
  end
    
module MakeTS (RT : RrType.R)
	      (PT : PriType.P with type t = RT.t list)
	      (S : S)
	      (G : G with type edge_type = RT.edge) :
sig

  module R :
  sig
    type t = RT.t
    type label = RT.label
    type occ = RT.occ
    val lhs : t -> Big.bg
    val rhs : t -> Big.bg
    val l : t -> label
    val map : t -> int Fun.t option
    val to_string : t -> string
    val is_valid : t -> bool
    val is_enabled : Big.bg -> t -> bool
    val fix : Big.bg -> t list -> Big.bg * int
    val step : Big.bg -> t list -> occ list * int
  end
    
  type p_class =
    PriType.Make(R)(PT).p_class =
      P_class of R.t list
    | P_rclass of R.t list

  val is_valid : p_class -> bool
  val is_valid_list : p_class list -> bool
  val rewrite : Big.bg -> int -> p_class list -> Big.bg * int
							    
  type t = G.t

  type stats = S.t

  exception MAX of t * stats

  val bfs :
    s0:Big.bg ->
    priorities:p_class list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> t * S.t

  val to_prism : G.t -> string

  val to_dot : G.t -> string

  val to_lab : G.t -> string

  val iter_states : f:(int -> Big.bg -> unit) -> G.t -> unit

  val write_svg : G.t -> name:string -> path:string -> int

  val write_prism : G.t -> name:string -> path:string -> int

  val write_lab : G.t -> name:string -> path:string -> int

  val write_dot : G.t -> name:string -> path:string -> int
end
