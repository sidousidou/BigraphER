module type G = sig
    type t
    type edge_type
    val init : int -> t
    val states : t -> (Big.bg_key, int * Big.bg) Hashtbl.t
    val label : t -> (int, int) Hashtbl.t
    val edges : t -> (int, edge_type) Hashtbl.t
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
end

module type S = sig
    type t
    type g
    val make : float -> g -> int -> t
  end
		  
module Make (R : RrType.T)
	      (P : sig
		  type p_class =
		    | P_class of R.t list
		    | P_rclass of R.t list
		  val is_valid : p_class -> bool
		  val is_valid_list : p_class list -> bool
		  val rewrite : Big.bg -> p_class list -> Big.bg * int
		  val scan : Big.bg * int ->
			     part_f:(R.occ list ->
				     ((int * R.occ) list * R.edge list * int)) ->
			     const_pri:p_class list -> p_class list ->
			     ((int * R.occ) list * R.edge list * int) * int
		  val scan_sim : Big.bg ->
				 iter_f:(int -> Big.bg -> unit) ->
				 const_pri:p_class list -> p_class list ->
				 R.occ option * int
		end)
	      (L : L with type occ = R.occ)
	      (G : G with type edge_type = R.edge)
	      (S : S with type g = G.t): sig
    
    type t = G.t

    type p_class = P.p_class =
		 | P_class of R.t list
		 | P_rclass of R.t list
	       
    type limit = L.t
		   
    exception MAX of t * S.t
		   
    exception LIMIT of t * S.t

    exception DEADLOCK of t * S.t * limit

    val to_string_react : R.t -> string
			    
    val is_valid_react : R.t -> bool
			   
    val fix : Big.bg -> R.t list -> Big.bg * int
		
    val step : Big.bg -> R.t list -> R.occ list * int
		 
    val random_step : Big.bg -> R.t list -> R.occ option * int
		    
    val is_valid_priority : p_class -> bool
			  
    val is_valid_priority_list : p_class list -> bool

    val rewrite : Big.bg -> p_class list -> Big.bg * int
				      
    val bfs :
      s0:Big.bg ->
      priorities:P.p_class list ->
      max:int -> iter_f:(int -> Big.bg -> unit) -> t * S.t

    val sim :
      s0:Big.bg ->
      priorities:P.p_class list -> init_size:int ->
      stop:limit -> iter_f:(int -> Big.bg -> unit) -> t * S.t

    val to_prism : t -> string

    val to_dot : t -> name: string -> string

    val to_lab : t -> string

    val iter_states : f:(int -> Big.bg -> unit) -> t -> unit

    val write_svg : t -> name:string -> path:string -> int

    val write_prism : t -> name:string -> path:string -> int

    val write_lab : t -> name:string -> path:string -> int

    val write_dot : t -> name:string -> path:string -> int

  end
