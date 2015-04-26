module type G =
  sig
    type t
    type edge_type
    type f
    type stats
    type limit
    type p_class
    val init : int -> t
    val states : t -> (Big.bg_key, int * Big.bg) Hashtbl.t
    val label : t -> (int, int) Hashtbl.t
    val edges : t -> (int, edge_type) Hashtbl.t
    val dest : edge_type -> int
    val arrow : edge_type -> f
    val string_of_arrow : edge_type -> string
  end
module type T =
  sig
    type t
    type stats
    type limit
    type p_class
    exception LIMIT of t * stats
    val bfs :
      Big.bg -> p_class list -> int -> (int -> Big.bg -> unit) -> t * stats
    val sim :
      Big.bg -> p_class list -> limit -> (int -> Big.bg -> unit) -> t * stats
    val to_prism : t -> string
    val to_lab : t -> string
    val to_dot : t -> string
    val iter_states : (int -> Big.bg -> unit) -> t -> unit
  end
module Make :
  functor (G : G) ->
    sig
      type t = G.t
      val to_prism : G.t -> string
      val to_dot : G.t -> string
      val to_lab : G.t -> string
      val iter_states : (int -> Big.bg -> unit) -> G.t -> unit
    end
