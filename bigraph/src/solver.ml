(* Solver type *)
type solver_t = MSAT | MCARD

(* Solver type wrapper*)
module type ST = sig
  val solver_type : solver_t

  val string_of_solver_t : string
end

(* Shared types *)
type value = False | True | Unknown

type solution = SAT | UNSAT

type stats = {
  v : int;  (** Number of variables. *)
  c : int;  (** Number of clauses. *)
  mem : float;  (** Memory used in MB. *)
  cpu : float;  (** CPU time in seconds. *)
}

let string_of_value = function
  | False -> "false"
  | True -> "true"
  | Unknown -> "unknown"

(* External solver interface *)
module type E = sig
  type t

  type var

  type lit

  val create : unit -> t

  val add_clause : t -> lit list -> unit

  val add_at_most : t -> lit list -> int -> unit

  val add_at_least : t -> lit list -> int -> unit

  val new_var : t -> var

  val dummy_var : var

  val simplify : t -> unit

  val solve : t -> solution

  val value_of : t -> var -> value

  val get_stats : t -> stats

  val positive_lit : var -> lit

  val negative_lit : var -> lit

  val negate : lit -> lit
end

(* Wrapper for the Minisat module *)
module MS_W : E = struct
  type t = Minisat.solver

  type var = Minisat.var

  type lit = Minisat.lit

  let dummy_var = Minisat.dummy_var

  let create () = Minisat.(new solver)

  let add_clause s lits = s#add_clause lits

  let new_var s = s#new_var

  let simplify s = s#simplify

  let solve s =
    match s#solve with Minisat.SAT -> SAT | Minisat.UNSAT -> UNSAT

  let value_of s v =
    match s#value_of v with
    | Minisat.False -> False
    | Minisat.True -> True
    | Minisat.Unknown -> Unknown

  let get_stats s =
    let x = s#get_stats in
    {
      v = Minisat.(x.v);
      c = Minisat.(x.c);
      mem = Minisat.(x.mem);
      cpu = Minisat.(x.cpu);
    }

  let positive_lit = Minisat.(pos_lit)

  let negative_lit = Minisat.(neg_lit)

  let negate = Minisat.(negate)

  let add_at_most s lits k =
    assert (k >= 0);
    assert false

  (* TO DO *)

  let add_at_least s lits k =
    assert (k >= 0);
    assert false

  (* TO DO *)
end

(* Wrapper for the Minicard module *)
module MC_W : E = struct
  type t = Minicard.solver

  type var = Minicard.var

  type lit = Minicard.lit

  let dummy_var = Minicard.dummy_var

  let create () = Minicard.(new solver)

  let add_clause s lits = s#add_clause lits

  let new_var s = s#new_var

  let simplify s = s#simplify

  let solve s =
    match s#solve with Minicard.SAT -> SAT | Minicard.UNSAT -> UNSAT

  let value_of s v =
    match s#value_of v with
    | Minicard.False -> False
    | Minicard.True -> True
    | Minicard.Unknown -> Unknown

  let get_stats s =
    let x = s#get_stats in
    {
      v = Minicard.(x.v);
      c = Minicard.(x.c);
      mem = Minicard.(x.mem);
      cpu = Minicard.(x.cpu);
    }

  let positive_lit = Minicard.(pos_lit)

  let negative_lit = Minicard.(neg_lit)

  let negate = Minicard.(negate)

  let add_at_most s lits k =
    assert (k >= 0);
    s#add_at_most lits k

  let add_at_least s lits k =
    assert (k >= 0);
    s#add_at_least lits k
end

module type S = sig
  include ST

  include E

  val new_var_vector : t -> int -> var array

  val new_var_matrix : t -> int -> int -> var array array

  val add_clauses : t -> lit list list -> unit

  val add_implication : t -> lit -> lit list list -> unit

  val add_iff : t -> lit -> lit list list -> unit

  val add_conj_pairs : t -> (lit * lit) list -> unit

  val add_exactly : t -> lit list -> int -> unit

  val add_fun : t -> var array array -> unit

  val add_injection : t -> var array array -> unit

  val ban : t -> var list -> unit

  val get_iso : t -> var array array -> Iso.t

  val get_fun : t -> var array array -> Fun.t
end

module Make (ST : ST) (E : E) : S = struct
  include ST
  include E

  let new_var_vector s n =
    assert (n >= 0);
    let vars = Array.make n dummy_var in
    for i = 0 to n - 1 do
      vars.(i) <- new_var s
    done;
    vars

  let new_var_matrix s m n =
    assert (m >= 0);
    assert (n >= 0);
    let vars = Array.make_matrix m n dummy_var in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        vars.(i).(j) <- new_var s
      done
    done;
    vars

  let add_clauses s = List.iter (fun c -> add_clause s c)

  (* Add x => (C && C && ...) *)
  let add_implication s x = List.iter (fun c -> add_clause s (negate x :: c))

  (* Add x <=> (C && C && ...) *)
  let add_iff s x clauses =
    List.flatten clauses |> List.iter (fun l -> add_clause s [ x; negate l ]);
    add_implication s x clauses

  (* Add (a && b) || (c && d) || ... via tseitin transformation *)
  (* Return auxiliary vars? *)
  let add_conj_pairs s = function
    | [] -> ()
    | [ (x, y) ] -> add_clauses s [ [ x ]; [ y ] ]
    | [ (x, y); (w, z) ] ->
        add_clauses s [ [ x; w ]; [ x; z ]; [ y; w ]; [ y; z ] ]
    | [ (x, y); (w, z); (h, k) ] ->
        let a0, a1, a2 = (new_var s, new_var s, new_var s) in
        add_clauses s
          [
            [ positive_lit a0; positive_lit a1; positive_lit a2 ];
            [ negative_lit a0; x ];
            [ negative_lit a0; y ];
            [ negative_lit a1; w ];
            [ negative_lit a1; z ];
            [ negative_lit a2; h ];
            [ negative_lit a2; k ];
          ]
    | [ (x, y); (w, z); (h, k); (u, v) ] ->
        let a0, a1, a2, a3 = (new_var s, new_var s, new_var s, new_var s) in
        add_clauses s
          [
            [
              positive_lit a0;
              positive_lit a1;
              positive_lit a2;
              positive_lit a3;
            ];
            [ negative_lit a0; x ];
            [ negative_lit a0; y ];
            [ negative_lit a1; w ];
            [ negative_lit a1; z ];
            [ negative_lit a2; h ];
            [ negative_lit a2; k ];
            [ negative_lit a3; u ];
            [ negative_lit a3; v ];
          ]
    | l ->
        let a, clauses =
          List.fold_left
            (fun (acc_a, acc) (x, y) ->
              let a = new_var s in
              ( positive_lit a :: acc_a,
                [ negative_lit a; x ] :: [ negative_lit a; y ] :: acc ))
            ([], []) l
        in
        add_clauses s (a :: clauses)

  let add_exactly s lits k =
    add_at_least s lits k;
    add_at_most s lits k

  (* One true per row *)
  let add_fun s m =
    Base.list_of_rows m
    |> List.iter (fun c -> add_exactly s (List.map positive_lit c) 1)

  (* 1. One true per row 2. At most one true per column *)
  let add_injection s m =
    add_fun s m;
    Base.list_of_cols m
    |> List.iter (fun c -> add_at_most s (List.map positive_lit c) 1)

  let ban s vars =
    List.map
      (fun v ->
        match value_of s v with
        | True -> negative_lit v
        | False -> positive_lit v
        | Unknown -> assert false)
      vars
    |> add_clause s

  (* Generate an iso from a matrix of assignments *)
  let get_iso s m =
    Base.fold_matrix m
      (fun iso i j x ->
        match value_of s x with
        | True -> Iso.add i j iso
        | False | Unknown -> iso)
      Iso.empty

  (* Generate a function from a matrix of assignments *)
  let get_fun s m =
    Base.fold_matrix m
      (fun f i j x ->
        match value_of s x with
        | True -> Fun.add i j f
        | False | Unknown -> f)
      Fun.empty
end

(* Instance of MiniSat solver *)
module MS =
  Make
    (struct
      let solver_type = MSAT

      let string_of_solver_t = "MiniSAT"
    end)
    ((
    MS_W : E ))

(*Instance of MiniCARD solver *)
module MC =
  Make
    (struct
      let solver_type = MCARD

      let string_of_solver_t = "MiniCARD"
    end)
    ((
    MC_W : E ))

(* The type of a bigraph matching engine *)
module type M = sig
  exception NODE_FREE

  val solver_type : solver_t

  val string_of_solver_t : string

  type t = { nodes : Iso.t; edges : Iso.t; hyper_edges : Fun.t }

  val occurs : target:Big.t -> pattern:Big.t -> bool

  val occurrence : target:Big.t -> pattern:Big.t -> t option

  val occurrences : target:Big.t -> pattern:Big.t -> t list

  val equal : Big.t -> Big.t -> bool

  val equal_key : Big.t -> Big.t -> bool
end

(* Bigraph mathing engine based on solver S *)
module Match (S : S) : M = struct
  let solver_type = S.solver_type

  let string_of_solver_t = S.string_of_solver_t

  exception NODE_FREE

  exception NO_MATCH

  type t = { nodes : Iso.t; edges : Iso.t; hyper_edges : Fun.t }

  type sat_vars = {
    nodes : S.var array array;
    (* z0_rows : S.var array array;
     * z0_cols : S.var array array; *)
    edges : S.var array array;
    map_edges_r : Iso.t;
    map_edges_c : Iso.t;
    (* z1_rows : S.var array array;
     * z1_cols : S.var array array;
     * z2 : S.var array array;
     * z3 : S.var array array; *)
    hyp : S.var array array;
    map_hyp_r : Iso.t;
    map_hyp_c : Iso.t;
  }

  let rec filter_loop solver t p vars t_trans =
    S.simplify solver;
    match S.solve solver with
    | UNSAT -> raise_notrace NO_MATCH
    | SAT ->
        if
          Big.(
            Place.check_match ~target:t.p ~pattern:p.p t_trans
              (S.get_iso solver vars.nodes))
        then vars
        else (
          S.ban solver
            ( Array.to_list vars.nodes |> List.map Array.to_list
            |> List.flatten );
          S.ban solver
            ( Array.to_list vars.edges |> List.map Array.to_list
            |> List.flatten );
          filter_loop solver t p vars t_trans )

  (* True when p is not a match *)
  let quick_unsat t p =
    Big.(
      Nodes.size p.n > Nodes.size t.n
      || Sparse.entries p.p.Place.nn > Sparse.entries t.p.Place.nn
      || Nodes.not_sub p.n t.n
      || IntSet.cardinal (Place.leaves p.p)
         > IntSet.cardinal (Place.leaves t.p)
      || IntSet.cardinal (Place.orphans p.p)
         > IntSet.cardinal (Place.orphans t.p)
      || Link.closed_edges p.l > Link.closed_edges t.l
      || Link.max_ports p.l > Link.max_ports t.l)

  let occurs ~target:t ~pattern:p =
    Big.(
      try
        if Nodes.size p.n = 0 then true
        else if quick_unsat t p then false
        else (
          ignore (aux_match t p (Place.trans t.p));
          true )
      with NO_MATCH -> false)

  let occurrence ~target:t ~pattern:p =
    Big.(
      if Nodes.size p.n = 0 then raise NODE_FREE
      else if quick_unsat t p then None
      else
        try
          let s, vars = aux_match t p (Place.trans t.p) in
          Some
            {
              nodes = S.get_iso s vars.nodes;
              edges =
                Iso.transform ~iso_dom:vars.map_edges_r
                  ~iso_codom:vars.map_edges_c (S.get_iso s vars.edges);
              hyper_edges =
                Fun.transform ~iso_dom:vars.map_hyp_r
                  ~iso_codom:vars.map_hyp_c (S.get_fun s vars.hyp);
            }
        with NO_MATCH -> None)

  let occurrences ~target ~pattern = assert false

  let equal a b = assert false

  let equal_key a b = assert false
end
