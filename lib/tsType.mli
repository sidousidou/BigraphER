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

module type L = sig
    type t
    val is_greater : t -> t -> bool
  end

module type S =
  sig
    type t
    val init : t0:float -> t
    val update : time:float -> states:int -> reacts:int -> occs:int ->
		 old_stats:t -> t
  end
    
module MakeTS (R : RrType.T)
	      (P : sig
		  type p_class =
		    | P_class of R.t list
		    | P_rclass of R.t list
		  val is_valid : p_class -> bool
		  val is_valid_list : p_class list -> bool
		  val rewrite : Big.bg -> int -> p_class list -> Big.bg * int
		  val scan : Big.bg * int -> matches:int ->
			     part_f:(R.occ list ->
				     ((int * R.occ) list * R.edge list * int)) ->
			     const_pri:p_class list -> p_class list ->
			     ((int * R.occ) list * R.edge list * int) * int
		end)
	      (S : S)
	      (G : G with type edge_type = R.edge) :
sig
  
  type t = G.t

  type stats = S.t

  exception MAX of t * stats

  val bfs :
    s0:Big.bg ->
    priorities:P.p_class list ->
    max:int -> iter_f:(int -> Big.bg -> unit) -> t * S.t

  val to_prism : G.t -> string

  val to_dot : G.t -> name: string -> string

  val to_lab : G.t -> string

  val iter_states : f:(int -> Big.bg -> unit) -> G.t -> unit

  val write_svg : G.t -> name:string -> path:string -> int

  val write_prism : G.t -> name:string -> path:string -> int

  val write_lab : G.t -> name:string -> path:string -> int

  val write_dot : G.t -> name:string -> path:string -> int

end

(* module MakeTrace (R : RrType.T) *)
(* 		 (P : sig *)
(* 		     type p_class = *)
(* 		       | P_class of R.t list *)
(* 		       | P_rclass of R.t list *)
(* 		     val is_valid : p_class -> bool *)
(* 		     val is_valid_list : p_class list -> bool *)
(* 		     val rewrite : Big.bg -> int -> p_class list -> Big.bg * int *)
(* 		   end) *)
(* 		 (S : S) *)
(* 		 (G : G with type edge_type = R.edge) : *)
(* sig *)

(*   type t = G.t *)
	     
(*   type stats = S.t *)

(*   exception MAX of t * stats *)

(*   val sim : *)
(*     s0:Big.bg -> *)
(*     priorities:P.p_class list -> *)
(*     max:int -> iter_f:(int -> Big.bg -> unit) -> t * S.t *)
						       
  
(*   val to_prism : G.t -> string *)
			  
(*   val to_dot : G.t -> string *)

(*   val to_lab : G.t -> string *)
			
(*   val iter_states : f:(int -> Big.bg -> unit) -> G.t -> unit *)

(*   val write_svg : G.t -> name:string -> path:string -> int *)
							 
(*   val write_prism : G.t -> name:string -> path:string -> int *)
							   
(*   val write_lab : G.t -> name:string -> path:string -> int *)

(*   val write_dot : G.t -> name:string -> path:string -> int *)
 
(* end *)

  
