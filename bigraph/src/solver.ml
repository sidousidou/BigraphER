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

  val add_exactly : t -> lit list -> int -> unit

  val new_var : t -> var

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

  (* Disjunctions (all possible pairs) of negative literals *)
  let add_at_most_naive s l =
    (* Boolean encoding of at most one TRUE. Most common cases are hard-coded *)
    let rec _at_most acc = function
      | [] | [ _ ] -> acc
      | [ a; b ] -> (a, b) :: acc
      | [ a; b; c ] -> (a, b) :: (a, c) :: (b, c) :: acc
      | [ a; b; c; d ] ->
          (a, b) :: (a, c) :: (a, d) :: (b, c) :: (b, d) :: (c, d) :: acc
      | [ a; b; c; d; e ] ->
          (a, b) :: (a, c) :: (a, d) :: (a, e) :: (b, c) :: (b, d) :: (b, e)
          :: (c, d) :: (c, e) :: (d, e) :: acc
      | [ a; b; c; d; e; f ] ->
          (a, b) :: (a, c) :: (a, d) :: (a, e) :: (a, f) :: (b, c) :: (b, d)
          :: (b, e) :: (b, f) :: (c, d) :: (c, e) :: (c, f) :: (d, e)
          :: (d, f) :: (e, f) :: acc
      | x :: rest -> _at_most (List.map (fun y -> (x, y)) rest @ acc) rest
    in
    List.iter
      (fun (a, b) -> add_clause s [ negate a; negate b ])
      (_at_most [] l)

  (* ++++++++++++++++++++ Commander variable encoding ++++++++++++++++++++ *)
  module CMD = struct
    type group = lit list

    type cmd_tree = Leaf of group | Node of (lit * cmd_tree) list

    (* Parameters t and g are used for configure the commander-variable
       encoding *)
    type params = { t : int; g : int }

    let defaults = { t = 6; g = 3 }

    (* Return a list of groups of size at most g + 1. If [0;1;2] [3] then
       [0;1] [2;3] to avoid singletons. Also if [0;1] [2] then [0;1;2] *)
    let group l n g =
      assert (g > 1);
      assert (n >= g);
      if List.length l <= n then None
      else
        let x, i, res =
          List.fold_left
            (fun (group, i, res) x ->
              if i >= g then ([ x ], 1, group :: res)
              else (x :: group, i + 1, res))
            ([], 0, []) l
        in
        if i = 1 then
          if g > 2 then
            match res with
            | (v :: vs) :: res -> Some ((x @ [ v ]) :: vs :: res)
            | _ -> assert false
          else
            match res with
            | v :: res -> Some ((x @ v) :: res)
            | _ -> assert false
        else Some (x :: res)

    (* Build a tree of commander variables. Input is a tree, output split the
       root and add a level of variables. *)
    let cmd_init l p s =
      let rec _cmd_init n g t =
        match t with
        | Node cmd_l -> (
            match group cmd_l n g with
            | Some cmd_l' ->
                Node
                  (List.fold_left
                     (fun acc g -> (positive_lit (new_var s), Node g) :: acc)
                     [] cmd_l')
                |> _cmd_init n g
            (* Do not add an additional level of commander variables *)
            | None -> t )
        | Leaf vars -> (
            match group vars n g with
            | Some cmd_l ->
                Node
                  (List.fold_left
                     (fun acc g -> (positive_lit (new_var s), Leaf g) :: acc)
                     [] cmd_l)
                |> _cmd_init n g
            (* Do not add an additional level of commander variables *)
            | None -> t )
      in
      _cmd_init p.t p.g (Leaf l)

    (* Scan the tree and add constraints: 1. at most one TRUE in every group
       2. if commander variable is TRUE then at least one TRUE in its group
       3. if commander variable is FALSE then no TRUE in its group 4. exactly
       one commander variable is true. *)

    (* [X0, X1, X2] -> [(!X0 or !X1), (!X0 or !X2), (!X1 or !X2)] *)
    let rec add_cmd_c1 s = function
      | Leaf g -> add_at_most_naive s g
      | Node cmd_g ->
          let cmd_vars, sub = List.split cmd_g in
          add_at_most_naive s cmd_vars;
          List.iter (fun t -> add_cmd_c1 s t) sub

    (* (C, [X0, X1, X2]) -> [!C or X0 or X1 or X2] *)
    let add_cmd_c2 s t =
      let rec aux = function
        | Leaf g -> g
        | Node cmd_g ->
            List.iter
              (fun (cmd_v, sub) -> add_clause s (negate cmd_v :: aux sub))
              cmd_g;
            fst (List.split cmd_g)
      in
      aux t |> ignore

    (* (C, [X0, X1, X2]) -> [(C or !X0), (C or !X1), (C or !X2)] *)
    let add_cmd_c3 s t =
      let rec aux = function
        | Leaf g -> g
        | Node cmd_g ->
            List.iter
              (fun (cmd_v, sub) ->
                aux sub
                |> List.iter (fun l -> add_clause s [ cmd_v; negate l ]))
              cmd_g;
            fst (List.split cmd_g)
      in
      aux t |> ignore

    let add_at_least_cmd s = function
      | Leaf g -> add_clause s g
      | Node cmd_g -> add_clause s (fst (List.split cmd_g))
  end

  (* Only base case implemented. *)
  let add_at_most s lits k =
    assert (k = 1);
    CMD.(
      cmd_init lits defaults s |> fun t ->
      add_cmd_c1 s t;
      add_cmd_c2 s t;
      add_cmd_c3 s t)

  (* Only base case implemented. *)
  let add_at_least s lits k =
    assert (k = 1);
    CMD.(cmd_init lits defaults s |> add_at_least_cmd s)

  (* Only base case implemented. *)
  let add_exactly s lits k =
    assert (k = 1);
    CMD.(
      cmd_init lits defaults s |> fun t ->
      add_cmd_c1 s t;
      add_cmd_c2 s t;
      add_cmd_c3 s t;
      add_at_least_cmd s t)
end

(* Wrapper for the Minicard module *)
module MC_W : E = struct
  type t = Minicard.solver

  type var = Minicard.var

  type lit = Minicard.lit

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

  let add_exactly s lits k =
    add_at_most s lits k;
    add_at_least s lits k
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

  val add_fun : t -> var array array -> unit

  val add_injection : t -> var array array -> unit

  val add_bijection : t -> var array array -> unit

  val ban : t -> var list -> unit

  val get_iso : t -> var array array -> Iso.t

  val get_fun : t -> var array array -> Fun.t
end

module Make (ST : ST) (E : E) : S = struct
  include ST
  include E

  let new_var_vector s n =
    assert (n >= 0);
    Base.list_n [] n (fun _ -> new_var s) |> Array.of_list

  let new_var_matrix s m n =
    assert (m >= 0);
    assert (n >= 0);
    Base.list_n [] m (fun _ -> new_var_vector s n) |> Array.of_list

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

  (* One true per row *)
  let add_fun s m =
    Base.list_of_rows m
    |> List.iter (fun c -> add_exactly s (List.map positive_lit c) 1)

  (* 1. One true per row 2. At most one true per column *)
  let add_injection s m =
    add_fun s m;
    Base.list_of_cols m
    |> List.iter (fun c -> add_at_most s (List.map positive_lit c) 1)

  (* 1. One true per row 2. One true per column *)
  let add_bijection s m =
    assert (Base.is_square_matrix m);
    add_fun s m;
    Base.list_of_cols m
    |> List.iter (fun c -> add_exactly s (List.map positive_lit c) 1)

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

  exception NOT_TOTAL

  val solver_type : solver_t

  val string_of_solver_t : string

  type t = { nodes : Iso.t; edges : Iso.t; hyper_edges : Fun.t }

  val occurs : target:Big.t -> pattern:Big.t -> bool

  val occurrence : target:Big.t -> pattern:Big.t -> t option

  val occurrence_memo : target:Big.t -> pattern:Big.t -> Sparse.t -> t option

  val occurrences : target:Big.t -> pattern:Big.t -> t list

  val equal : Big.t -> Big.t -> bool

  val equal_key : Big.t -> Big.t -> bool
end

(* Bigraph mathing engine based on solver S *)
module MatchSAT (S : S) : M = struct
  let solver_type = S.solver_type

  let string_of_solver_t = S.string_of_solver_t

  exception NODE_FREE

  exception NOT_TOTAL

  exception NO_MATCH

  type t = { nodes : Iso.t; edges : Iso.t; hyper_edges : Fun.t }

  type sat_vars = {
    nodes : S.var array array;
    edges : S.var array array;
    map_edges_r : Iso.t;
    map_edges_c : Iso.t;
    hyp : S.var array array;
    map_hyp_r : Iso.t;
    map_hyp_c : Iso.t;
  }

  let add_c11 unmatch_js solver m =
    IntSet.iter
      (fun j ->
        Array.iteri
          (fun i _ -> S.add_clause solver [ S.negative_lit m.(i).(j) ])
          m)
      unmatch_js

  (* Place graph constraints *)
  module P = struct
    open Place

    module H = Hashtbl.Make (struct
      type t = Ctrl.t * Ctrl.t

      let equal (a, b) (a', b') = Ctrl.equal a a' && Ctrl.equal b b'

      let hash = Hashtbl.hash
    end)

    (* Given an edge of control A -> B, find all the edges with the same
       type. return a hash table (ctrl * ctrl) -> (int * int) *)
    let partition_edges p n =
      let h = H.create (Sparse.entries p.nn) in
      Sparse.iter
        (fun i j ->
          match
            (Base.safe @@ Nodes.get_ctrl i n, Base.safe @@ Nodes.get_ctrl j n)
          with
          | c, c' -> H.add h (c, c') (i, j))
        p.nn;
      h

    type deg =
      | V of int
      (* only vertices *)
      | S of int

    (* with sites or regions *)

    let indeg p i =
      assert (i >= 0);
      let d = IntSet.cardinal (Sparse.prn p.nn i) in
      if IntSet.is_empty (Sparse.prn p.rn i) then V d else S d

    let outdeg p i =
      assert (i >= 0);
      let d = IntSet.cardinal (Sparse.chl p.nn i) in
      if IntSet.is_empty (Sparse.chl p.ns i) then V d else S d

    (* true if the degrees are compatible *)
    let compat_deg t p =
      match p with
      | V d -> ( match t with V d' -> d = d' | S _ -> false )
      | S d -> ( match t with V d' -> d' >= d | S d' -> d' >= d )

    let compat t p t_i p_i =
      compat_deg (indeg t t_i) (indeg p p_i)
      && compat_deg (outdeg t t_i) (outdeg p p_i)

    let eq t p t_i p_i =
      indeg t t_i = indeg p p_i && outdeg t t_i = outdeg p p_i

    (* Match nodes in compatible DAG edges *)
    let match_cmp ~target:t ~pattern:p ~n_t ~n_p cmp s m =
      let h = partition_edges t n_t in
      Sparse.fold
        (fun i j acc_c ->
          let a, b =
            ( Base.safe @@ Nodes.get_ctrl i n_p,
              Base.safe @@ Nodes.get_ctrl j n_p )
          in
          match
            List.filter
              (fun (i', j') ->
                (* Degree check *)
                cmp t p i' i && compat t p j' j)
              (H.find_all h (a, b))
          with
          | [] ->
              (* No compatible edges found, break out from fold *)
              raise_notrace NOT_TOTAL
          | t_edges ->
              List.map
                (fun (i', j') ->
                  (S.positive_lit m.(i).(i'), S.positive_lit m.(j).(j')))
                t_edges
              |> S.add_conj_pairs s;
              List.fold_left
                (fun acc (i', j') -> IntSet.add j' (IntSet.add i' acc))
                acc_c t_edges)
        p.nn IntSet.empty

    let add_c4 ~target:t ~pattern:p ~n_t ~n_p =
      match_cmp ~target:t ~pattern:p ~n_t ~n_p compat

    let add_c5 ~target:t ~pattern:p ~n_t ~n_p s m =
      let aux l_p l_t acc =
        IntSet.fold
          (fun i acc_c ->
            let c = Base.safe @@ Nodes.get_ctrl i n_p in
            let compat_t = IntSet.inter (Nodes.find_all c n_t) l_t in
            if IntSet.is_empty compat_t then raise_notrace NOT_TOTAL
            else (
              IntSet.fold
                (fun j acc -> S.positive_lit m.(i).(j) :: acc)
                compat_t []
              |> S.add_clause s;
              IntSet.union acc_c compat_t ))
          l_p acc
      in
      aux (orphans p) (orphans t) (aux (leaves p) (leaves t) IntSet.empty)

    let add_c6 ~target:t ~pattern:p ~n_t ~n_p s m =
      (* Only ctrl and deg check *)
      let aux adj acc =
        Sparse.fold
          (fun i _ acc_c ->
            let js =
              Base.(
                safe @@ Nodes.get_ctrl i n_p
                |> flip Nodes.find_all n_t
                |> IntSet.filter (fun j -> compat t p j i))
            in
            if IntSet.is_empty js then raise_notrace NOT_TOTAL
            else
              IntSet.fold
                (fun j acc -> S.positive_lit m.(i).(j) :: acc)
                js []
              |> S.add_clause s;
            IntSet.union js acc_c)
          adj acc
      in
      aux p.rn (aux p.ns IntSet.empty)

    (* Block unconnected pairs of nodes with sites and nodes with regions. *)
    let add_c10 ~target:t ~pattern:p s m =
      let n_s = Sparse.dom p.ns (* index i *) and n_r = Sparse.codom p.rn in
      (* index j *)
      let n_s', n_r' =
        IntSet.fold
          (fun i (acc_s, acc_r) ->
            let x = IntSet.inter (Sparse.chl p.nn i) n_r in
            if IntSet.cardinal x = 0 then (acc_s, acc_r)
            else (IntSet.remove i acc_s, IntSet.diff acc_r x))
          n_s (n_s, n_r)
      in
      (* For every edge (i', j') in t, block any ii' and jj' *)
      Sparse.iter
        (fun i' j' ->
          IntSet.iter
            (fun i ->
              IntSet.iter
                (fun j ->
                  S.add_clause s
                    [ S.negative_lit m.(i).(i'); S.negative_lit m.(j).(j') ])
                n_r')
            n_s')
        t.nn

    let check_sites t p v_p' c_set iso =
      let s_set =
        IntSet.fold
          (fun j acc ->
            let children = Sparse.chl t.ns j in
            IntSet.union acc children)
          v_p' IntSet.empty
      in
      (* Is there a set of sites with the same parent set? *)
      (* Nodes *)
      IntSet.for_all
        (fun c ->
          let prn_c = IntSet.inter v_p' (Sparse.prn t.nn c) in
          (* Construct a candidate set of sites *)
          let candidate =
            IntSet.fold
              (fun s acc ->
                let prn_s = IntSet.apply iso (Sparse.prn p.ns s) in
                if IntSet.subset prn_s prn_c then IntSet.union prn_s acc
                else acc)
              (IntSet.of_int p.s) IntSet.empty
          in
          (* Equality test *)
          IntSet.equal candidate prn_c)
        c_set
      && (* Sites *)
      IntSet.for_all
        (fun s ->
          let prn_s = IntSet.inter v_p' (Sparse.prn t.ns s) in
          (* Construct a candidate set of sites *)
          let candidate =
            IntSet.fold
              (fun s acc ->
                let prn_s' = IntSet.apply iso (Sparse.prn p.ns s) in
                if IntSet.subset prn_s' prn_s then IntSet.union prn_s' acc
                else acc)
              (IntSet.of_int p.s) IntSet.empty
          in
          (* Equality test *)
          IntSet.equal candidate prn_s)
        s_set

    (* Dual *)
    let check_regions t p v_p' iso =
      let p_set =
        IntSet.fold
          (fun j acc ->
            let parents = IntSet.diff (Sparse.prn t.nn j) v_p' in
            IntSet.union acc parents)
          v_p' IntSet.empty
      and r_set =
        IntSet.fold
          (fun j acc ->
            let parents = Sparse.prn t.rn j in
            IntSet.union acc parents)
          v_p' IntSet.empty
      in
      (* Is there a set of regions with the same children set? *)
      (* Nodes *)
      IntSet.for_all
        (fun x ->
          let chl_p = IntSet.inter v_p' (Sparse.chl t.nn x) in
          (* Construct a candidate set of regions *)
          let candidate =
            IntSet.fold
              (fun r acc ->
                let chl_r = IntSet.apply iso (Sparse.chl p.rn r) in
                if IntSet.subset chl_r chl_p then IntSet.union chl_r acc
                else acc)
              (IntSet.of_int p.r) IntSet.empty
          in
          (* Equality test *)
          IntSet.equal candidate chl_p)
        p_set
      && (* Regions *)
      IntSet.for_all
        (fun x ->
          let chl_r = IntSet.inter v_p' (Sparse.chl t.rn x) in
          (* Construct a candidate set of regions *)
          let candidate =
            IntSet.fold
              (fun r acc ->
                let chl_r' = IntSet.apply iso (Sparse.chl p.rn r) in
                if IntSet.subset chl_r' chl_r then IntSet.union chl_r' acc
                else acc)
              (IntSet.of_int p.r) IntSet.empty
          in
          (* Equality test *)
          IntSet.equal candidate chl_r)
        r_set

    (* check TRANS *)
    let check_trans t_trans v_p' c_set =
      (* check if there is a node child of co-domain, outside co-domain, such
         that one of its children in trans is in co-domain *)
      not
        (IntSet.exists
           (fun c ->
             IntSet.exists
               (fun t -> IntSet.mem t v_p')
               (Sparse.chl t_trans c))
           c_set)

    (* If there is an edge in the target between two nodes in the co-domain
       then there must be an edge in the pattern between the corresponding
       nodes in the domain. *)
    let check_edges t p v_p' iso =
      let iso' = Iso.inverse iso in
      IntSet.fold
        (fun u acc ->
          IntSet.fold
            (fun v acc -> (u, v) :: acc)
            (IntSet.inter v_p' (Sparse.chl t.nn u))
            acc)
        v_p' []
      |> List.for_all (fun (u, v) ->
             Sparse.mem p.nn
               (Base.safe @@ Iso.apply iso' u)
               (Base.safe @@ Iso.apply iso' v))

    (* Check if iso i : p -> t is valid *)
    let check_match ~target:t ~pattern:p t_trans iso =
      let v_p' = IntSet.of_list (Iso.codom iso) in
      let c_set =
        IntSet.fold
          (fun j acc ->
            IntSet.diff (Sparse.chl t.nn j) v_p' |> IntSet.union acc)
          v_p' IntSet.empty
      in
      check_sites t p v_p' c_set iso
      && check_regions t p v_p' iso
      && check_edges t p v_p' iso
      && check_trans t_trans v_p' c_set

    (* ++++++++++++++++++++++ Equality functions ++++++++++++++++++++++ *)

    let deg_regions p =
      IntSet.fold
        (fun r acc -> IntSet.cardinal (Sparse.chl p.rn r) :: acc)
        (IntSet.of_int p.r) []

    let deg_sites p =
      IntSet.fold
        (fun s acc -> IntSet.cardinal (Sparse.prn p.ns s) :: acc)
        (IntSet.of_int p.s) []

    (* match_list_eq *)
    let add_c4_eq a b n_a n_b s m =
      ignore (match_cmp ~target:a ~pattern:b ~n_t:n_a ~n_p:n_b eq s m)

    let add_region_nodes a b n_a n_b s m =
      Sparse.iter
        (fun r i ->
          let c = Base.safe @@ Nodes.get_ctrl i n_a in
          let children =
            IntSet.filter
              (fun i -> Ctrl.equal c (Base.safe @@ Nodes.get_ctrl i n_b))
              (Sparse.chl b.rn r)
          in
          IntSet.fold
            (fun j acc -> S.positive_lit m.(i).(j) :: acc)
            children []
          |> S.add_clause s)
        a.rn

    (*Dual*)
    let add_nodes_sites a b n_a n_b solver m =
      Sparse.iter
        (fun i s ->
          let c = Base.safe @@ Nodes.get_ctrl i n_a in
          let parents =
            IntSet.filter
              (fun i -> Ctrl.equal c (Base.safe @@ Nodes.get_ctrl i n_b))
              (Sparse.prn b.ns s)
          in
          IntSet.fold
            (fun j acc -> S.positive_lit m.(i).(j) :: acc)
            parents []
          |> S.add_clause solver)
        a.ns
  end

  (* Link graph constraints *)
  module L = struct
    open Link

    let comp_multi (c, n) (c', n') =
      match Ctrl.compare c c' with 0 -> Base.int_compare n n' | x -> x

    let ports_type p n =
      Ports.fold
        (fun i mul acc -> (Base.safe @@ Nodes.get_ctrl i n, mul) :: acc)
        p []
      |> List.fast_sort comp_multi

    let is_closed e = Face.is_empty e.i && Face.is_empty e.o

    let equal_types (p, n) (p', n') =
      Base.list_equal
        (fun (c, n) (c', n') -> Ctrl.equal c c' && n = n')
        (ports_type p n) (ports_type p' n')

    let inter a b =
      List.fold_left
        (fun (acc, b') (c, n) ->
          match
            List.partition (fun (c', n') -> Ctrl.equal c c' && n' >= n) b'
          with
          | [], _ -> (acc, b')
          | _ :: l, l' -> ((c, n) :: acc, l @ l'))
        ([], b) a
      |> fst |> List.fast_sort comp_multi

    (* Two edges are compatible if they have the same number of ports with
       equal control. *)
    let compat_edges e_p e_t n_t n_p = equal_types (e_p.p, n_p) (e_t.p, n_t)

    (* Closed edges in p are matched to closed edges in t. Controls are
       checked to exclude incompatible pairs. Return blocking pairs and
       blocking columns. *)
    let match_edges ~target ~pattern ~n_t ~n_p =
      let clauses, _, acc_c, acc_b =
        Lg.fold
          (fun e_p (acc, i, acc_c, acc_b) ->
            let clause, js, b, _ =
              Lg.fold
                (fun e_t (acc, js, b, j) ->
                  if compat_edges e_p e_t n_t n_p then
                    ((i, j) :: acc, j :: js, b, j + 1)
                  else (acc, js, (i, j) :: b, j + 1))
                target ([], [], [], 0)
            in
            match js with
            | [] -> raise_notrace NOT_TOTAL (* No compatible edges found *)
            | _ -> (clause :: acc, i + 1, acc_c @ js, acc_b @ b))
          pattern ([], 0, [], [])
      in
      ( clauses,
        IntSet.diff
          (IntSet.of_int (Lg.cardinal target))
          (IntSet.of_list acc_c),
        acc_b )

    let add_c7 ~target ~pattern ~n_t ~n_p s w =
      let clauses, b_cols, b_pairs =
        match_edges ~target ~pattern ~n_t ~n_p
      in
      List.map (List.map (fun (i, j) -> S.positive_lit w.(i).(j))) clauses
      |> S.add_clauses s;
      List.map (fun (i, j) -> [ S.negative_lit w.(i).(j) ]) b_pairs
      |> S.add_clauses s;
      add_c11 b_cols s w;
      clauses

    let compat_list (a : Ports.t) (b : Ports.t) n_a n_b =
      let i_a = Ports.to_IntSet a and i_b = Ports.to_IntSet b in
      IntSet.fold
        (fun i acc ->
          let ar_i = Base.safe @@ Ports.arity a i
          and c_i = Base.safe @@ Nodes.get_ctrl i n_a in
          let pairs =
            IntSet.filter
              (fun j ->
                ar_i = Base.safe @@ Ports.arity b j
                && Ctrl.equal c_i (Base.safe @@ Nodes.get_ctrl j n_b))
              i_b
            |> IntSet.elements
            |> List.map (fun j -> (i, j))
          in
          pairs :: acc)
        i_a []

    (* Nodes of matched edges are isomorphic. Indexes in clauses are for
       closed edges. *)
    let add_c8 ~target ~pattern ~n_t ~n_p solver v w clauses =
      let a_t =
        Lg.elements target |> List.map (fun e -> e.p) |> Array.of_list
      and a_p =
        Lg.elements pattern |> List.map (fun e -> e.p) |> Array.of_list
      in
      List.iter
        (fun (e_i, e_j) ->
          compat_list a_p.(e_i) a_t.(e_j) n_p n_t
          |> List.map (List.map (fun (i, j) -> S.positive_lit v.(i).(j)))
          |> S.add_implication solver (S.negative_lit w.(e_i).(e_j)))
        (List.flatten clauses)

    (* Is p sub-hyperedge of t? *)
    let sub_edge p t n_t n_p =
      let p_l = ports_type p n_p and t_l = ports_type t n_t in
      inter p_l t_l = p_l

    (* Return a list of clauses on row i of matrix t. Cnf.impl will process
       each element *)
    let compat_clauses e_p i t h_t n_t n_p =
      let p = Ports.to_IntSet e_p.p in
      IntSet.fold
        (fun j acc ->
          let e_t = Base.safe @@ Base.H_int.find h_t j in
          let clauses =
            IntSet.fold
              (fun v acc ->
                let c_v = Base.safe @@ Nodes.get_ctrl v n_p
                and arity_v = Base.safe @@ Ports.arity e_p.p v
                and p_t = Ports.to_IntSet e_t.p in
                (* find nodes in e_t that are compatible with v *)
                let compat_t =
                  IntSet.filter
                    (fun u ->
                      Ctrl.equal c_v (Base.safe @@ Nodes.get_ctrl u n_t)
                      && arity_v <= Base.safe @@ Ports.arity e_t.p u)
                    p_t
                in
                let nodes_assign =
                  IntSet.fold (fun j acc -> (v, j) :: acc) compat_t []
                in
                nodes_assign :: acc)
              p []
          in
          ((i, j), clauses) :: acc)
        t []

    let port_subsets p_i_list j p_a t_edge n_t n_p =
      let subsets xs =
        List.fold_right
          (fun x rest -> rest @ List.map (fun ys -> x :: ys) rest)
          xs [ [] ]
      in
      let blocks =
        List.filter
          (fun l ->
            let p_set =
              List.fold_left
                (fun acc i -> Ports.sum acc p_a.(i).p)
                Ports.empty l
            in
            not (sub_edge p_set t_edge.p n_t n_p))
          (subsets p_i_list)
      in
      List.map (fun l -> List.map (fun i -> (i, j)) l) blocks

    (* Generate constraints to block sets of edges in P that cannot be
       matched to a link in T. Example: {A, B} -> [{A}, {B}, {A, B}] blocks
       [{A}, {A, B}], [{B}, {A, B}] and [{A}, {B}, {A, B}] *)

    (* 3 -> 0,2,4 *)
    (* !(0,3) | ! (4,3) | !(2,3) *)
    (* !(0,3) | ! (4,3) *)
    (* !(2,3) | ! (4,3) *)

    let compat_sub p t f_e n_t n_p =
      let p_a = Array.of_list (Lg.elements p)
      and t_a = Array.of_list (Lg.elements t) in
      Base.H_int.fold
        (fun j _ (acc, marked) ->
          if List.mem j marked then (acc, marked)
          else
            let p_i_list = Base.H_int.find_all f_e j in
            let p_set =
              List.fold_left
                (fun acc i -> Ports.sum acc p_a.(i).p)
                Ports.empty p_i_list
            in
            if sub_edge p_set t_a.(j).p n_t n_p then (acc, marked)
            else
              let clauses = port_subsets p_i_list j p_a t_a.(j) n_t n_p in
              (clauses @ acc, j :: marked))
        f_e ([], [])
      |> fst

    let filter_iso f l =
      let l', _, _, iso =
        Lg.fold
          (fun e (acc, i, i', iso) ->
            if f e then (Lg.add e acc, i + 1, i' + 1, Iso.add i' i iso)
            else (acc, i + 1, i', iso))
          l
          (Lg.empty, 0, 0, Iso.empty)
      in
      (l', iso)

    (* Peers in the pattern are peers in the target. Auxiliary variables are
       introduced to model open edges matchings. They are stored in matrix t *)
    let add_c9 ~target ~pattern ~n_t ~n_p solver v =
      let open_p, iso_p =
        filter_iso
          (fun e -> (not (Ports.is_empty e.p)) && not (is_closed e))
          pattern
      and non_empty_t, iso_open =
        filter_iso (fun e -> not (Ports.is_empty e.p)) target
      in
      let h = Base.H_int.create (Lg.cardinal non_empty_t) in
      ignore
        (Lg.fold
           (fun e i ->
             Base.H_int.add h i e;
             i + 1)
           non_empty_t 0);
      let r = Lg.cardinal open_p and c = Lg.cardinal non_empty_t in
      let w = S.new_var_matrix solver r c in
      S.add_fun solver w;
      let f_e = Base.H_int.create (r * c) in
      (* T -> P *)
      let c_s = IntSet.of_int c in
      Lg.fold
        (fun e_p i ->
          (* Find compatible edges in the target *)
          let _, compat_t =
            Lg.fold
              (fun e_t (j, acc) ->
                if sub_edge e_p.p e_t.p n_t n_p then (
                  Base.H_int.add f_e j i;
                  (j + 1, IntSet.add j acc) )
                else (j + 1, acc))
              non_empty_t (0, IntSet.empty)
          in
          (* No compatible edges found *)
          if IntSet.is_empty compat_t then raise_notrace NOT_TOTAL
          else (
            (* Blockig pairs *)
            IntSet.iter
              (fun j -> S.add_clause solver [ S.negative_lit w.(i).(j) ])
              (IntSet.diff c_s compat_t);
            (* Generate possible node matches for every edge assignment. *)
            List.iter
              (fun ((l0, l1), r) ->
                List.map
                  (List.map (fun (a, b) -> S.positive_lit v.(a).(b)))
                  r
                |> S.add_implication solver (S.positive_lit w.(l0).(l1)))
              (compat_clauses e_p i compat_t h n_t n_p);
            i + 1 ))
        open_p 0
      |> ignore;
      compat_sub open_p non_empty_t f_e n_t n_p
      |> List.map (List.map (fun (i, j) -> S.negative_lit w.(i).(j)))
      |> S.add_clauses solver;
      (w, iso_p, iso_open)

    (* Block columns in W' when the corresponding column in W is in a match *)
    let add_c12 solver w iso_w w' iso_w' =
      (* T index -> W' index *)
      let inv_w' = Iso.inverse iso_w' in
      let convert_j j =
        Base.safe (Iso.apply inv_w' (Base.safe (Iso.apply iso_w j)))
      and vars_of_col j m =
        snd
          (Array.fold_left
             (fun (i, acc) _ -> (i + 1, S.negative_lit m.(i).(j) :: acc))
             (0, []) m)
      in
      let cols_w = Iso.dom iso_w in
      List.iter
        (fun j ->
          let vars_w = vars_of_col j w
          and vars_w' = vars_of_col (convert_j j) w' in
          Base.cartesian vars_w vars_w'
          |> List.iter (fun (j, j') -> S.add_clause solver [ j; j' ]))
        cols_w

    let edg_iso a b n_a n_b =
      Face.equal a.i b.i && Face.equal a.o b.o
      && equal_types (a.p, n_a) (b.p, n_b)

    let key e = (Face.cardinal e.i, Ports.cardinal e.p, Face.cardinal e.o)

    module H_3 = Hashtbl.Make (struct
      type t = int * int * int

      let equal (x : int * int * int) y = x = y

      let hash = Hashtbl.hash
    end)

    (* Partition edges according to cardinalities of faces and port sets.
       Return a hastbl : key -> (edge, index) *)
    let partition_edg l =
      let h = H_3.create (Lg.cardinal l) in
      ignore
        (Lg.fold
           (fun e i ->
             let k = key e in
             H_3.add h k (e, i);
             i + 1)
           l 0);
      h

    let block_rows rows m =
      List.fold_left
        (fun acc i ->
          Array.to_list m.(i)
          |> List.map S.negative_lit |> Base.flip List.cons acc)
        [] rows

    (* P -> T Example constraint: [[(1, 2) or (1, 3) or (1, 4)]; [(2, 4)];
       [(3, 4) or (3, 2)]] *)
    let match_list_eq p t n_p n_t solver v_l =
      let h = partition_edg t in
      let clauses, b, _ =
        Lg.fold
          (fun e_p (acc, block, i) ->
            let t_edges = H_3.find_all h (key e_p) in
            let clause =
              List.fold_left
                (fun acc (e_t, j) ->
                  if edg_iso e_t e_p n_t n_p then (i, j) :: acc else acc)
                [] t_edges
            in
            match clause with
            | [] -> (acc, i :: block, i + 1)
            | _ -> (clause :: acc, block, i + 1))
          p ([], [], 0)
      in
      S.add_clauses solver
        ( List.map
            (List.map (fun (i, j) -> S.positive_lit v_l.(i).(j)))
            clauses
        @ block_rows b v_l );
      clauses

    let match_ports_eq p t n_p n_t solver v w clauses =
      let a_t = Array.of_list (List.map (fun e -> e.p) (Lg.elements t))
      and a_p = Array.of_list (List.map (fun e -> e.p) (Lg.elements p)) in
      List.iter
        (fun (e_i, e_j) ->
          compat_list a_p.(e_i) a_t.(e_j) n_p n_t
          |> List.map (List.map (fun (i, j) -> S.positive_lit v.(i).(j)))
          |> S.add_implication solver (S.negative_lit w.(e_i).(e_j)))
        (List.flatten clauses)
  end

  let ban_solution solver vars =
    let aux m =
      S.ban solver (Array.to_list m |> List.map Array.to_list |> List.flatten)
    in
    aux vars.nodes;
    aux vars.edges

  let rec filter_loop solver t p vars t_trans =
    S.simplify solver;
    match S.solve solver with
    | UNSAT -> raise_notrace NO_MATCH
    | SAT ->
        if
          Big.(
            P.check_match ~target:t.p ~pattern:p.p t_trans
              (S.get_iso solver vars.nodes))
        then (solver, vars)
        else (
          ban_solution solver vars;
          filter_loop solver t p vars t_trans )

  (* Compute isos from nodes in the pattern to nodes in the target *)
  let aux_match t p t_trans =
    Big.(
      try
        let solver = S.create ()
        and n = p.p.Place.n
        and m = t.p.Place.n
        and closed_p, iso_w_r = Link.closed_edges_iso p.l
        and closed_t, iso_w_c = Link.closed_edges_iso t.l in
        let e = Link.Lg.cardinal closed_p
        and f = Link.Lg.cardinal closed_t in
        (* Variables for constraints between nodes *)
        let v = S.new_var_matrix solver n m
        (* Variables for constraints between closed edges *)
        and w = S.new_var_matrix solver e f in
        (* Add injection over nodes *)
        S.add_injection solver v;
        (* Add injection over closed edges *)
        S.add_injection solver w;
        (* Add Tseitin C4: ctrl, edges and degrees in the palce graphs.
           Return list of vectors of auxiliary vars. *)
        let js0 =
          P.add_c4 ~target:t.p ~pattern:p.p ~n_t:t.n ~n_p:p.n solver v
        (* Add C5: orphans and leaves matching in the place graphs. *)
        and js1 =
          P.add_c5 ~target:t.p ~pattern:p.p ~n_t:t.n ~n_p:p.n solver v
        (* Add C6: sites and regions in the place graphs. *)
        and js2 =
          P.add_c6 ~target:t.p ~pattern:p.p ~n_t:t.n ~n_p:p.n solver v
        in
        (* Add C7: edges in the pattern are matched to edges in the target. *)
        L.add_c7 ~target:closed_t ~pattern:closed_p ~n_t:t.n ~n_p:p.n solver
          w
        (* Add C8: ports of matched closed edges have to be isomorphic. *)
        |> L.add_c8 ~target:closed_t ~pattern:closed_p ~n_t:t.n ~n_p:p.n
             solver v w;
        (* Add C9: ports of matched open edges have to be isomorphic. Return
           matrix from open edges in the pattern to non-empty edges in the
           target. *)
        let w', iso_w'_r, iso_w'_c =
          L.add_c9 ~target:t.l ~pattern:p.l ~n_t:t.n ~n_p:p.n solver v
        in
        (* Add C10: block edges between unconnected nodes with sites and
           nodes with regions. *)
        P.add_c10 ~target:t.p ~pattern:p.p solver v;
        (* If an edge is in a match in w then forbid matches in w' *)
        L.add_c12 solver w iso_w_c w' iso_w'_c;
        (* Block unmatchable columns *)
        add_c11
          (IntSet.diff (IntSet.of_int m)
             (IntSet.union_list [ js0; js1; js2 ]))
          solver v;
        let vars =
          {
            nodes = v;
            edges = w;
            map_edges_r = iso_w_r;
            map_edges_c = iso_w_c;
            hyp = w';
            map_hyp_r = iso_w'_r;
            map_hyp_c = iso_w'_c;
          }
        in
        filter_loop solver t p vars t_trans
      with NOT_TOTAL -> raise_notrace NO_MATCH)

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

  let occurrence_memo ~target:t ~pattern:p trans =
    Big.(
      if Nodes.size p.n = 0 then raise NODE_FREE
      else if quick_unsat t p then None
      else
        try
          let s, vars = aux_match t p trans in
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

  let occurrence ~target ~pattern =
    occurrence_memo ~target ~pattern (Place.trans target.p)

  let clause_of_iso iso m =
    Base.fold_matrix m
      (fun acc i j v ->
        if Base.safe (Iso.apply iso i) = j then S.negative_lit v :: acc
        else S.positive_lit v :: acc)
      []

  (* compute non-trivial automorphisms of b *)
  let auto b =
    Big.(
      let b_trans = Place.trans b.p
      and rem_id res =
        List.filter (fun (i, e) -> not (Iso.is_id i && Iso.is_id e)) res
      in
      rem_id
        ( try
            let s, vars = aux_match b b b_trans in
            let rec loop_occur res =
              ban_solution s vars;
              try
                ignore (filter_loop s b b vars b_trans);
                loop_occur
                  ((S.get_iso s vars.nodes, S.get_iso s vars.edges) :: res)
              with NO_MATCH -> res
            in
            loop_occur [ (S.get_iso s vars.nodes, S.get_iso s vars.edges) ]
          with NO_MATCH -> [] ))

  let occurrences ~target:t ~pattern:p =
    Big.(
      if Nodes.size p.n = 0 then raise NODE_FREE
      else if quick_unsat t p then []
      else
        try
          let t_trans = Place.trans t.p in
          let s, vars = aux_match t p t_trans in
          let autos = auto p in
          let rec loop_occur res =
            ban_solution s vars;
            (****************AUTOMORPHISMS****************)
            List.combine
              (Iso.gen_isos (S.get_iso s vars.nodes) (List.map fst autos))
              (Iso.gen_isos (S.get_iso s vars.edges) (List.map snd autos))
            |> List.iter (fun (iso_i, iso_e) ->
                   S.add_clause s
                     ( clause_of_iso iso_i vars.nodes
                     @ clause_of_iso iso_e vars.edges ));
            (*********************************************)
            try
              ignore (filter_loop s t p vars t_trans);
              loop_occur
                ( {
                    nodes = S.get_iso s vars.nodes;
                    edges =
                      S.get_iso s vars.edges
                      |> Iso.transform ~iso_dom:vars.map_edges_r
                           ~iso_codom:vars.map_edges_c;
                    hyper_edges =
                      S.get_fun s vars.hyp
                      |> Fun.transform ~iso_dom:vars.map_hyp_r
                           ~iso_codom:vars.map_hyp_c;
                  }
                :: res )
            with NO_MATCH -> res
          in
          loop_occur
            [
              {
                nodes = S.get_iso s vars.nodes;
                edges =
                  S.get_iso s vars.edges
                  |> Iso.transform ~iso_dom:vars.map_edges_r
                       ~iso_codom:vars.map_edges_c;
                hyper_edges =
                  S.get_fun s vars.hyp
                  |> Fun.transform ~iso_dom:vars.map_hyp_r
                       ~iso_codom:vars.map_hyp_c;
              };
            ]
        with NO_MATCH -> [])

  let equal_SAT a b =
    Big.(
      try
        let solver = S.create () in
        let n = a.p.Place.n and h = Link.Lg.cardinal a.l in
        let v_n = S.new_var_matrix solver n n
        and v_l = S.new_var_matrix solver h h in
        S.add_bijection solver v_n;
        S.add_bijection solver v_l;
        (* Place graph *)
        P.add_c4_eq a.p b.p a.n b.n solver v_n;
        ignore
          (P.add_c5 ~target:b.p ~pattern:a.p ~n_t:b.n ~n_p:a.n solver v_n);
        P.add_region_nodes a.p b.p a.n b.n solver v_n;
        P.add_nodes_sites a.p b.p a.n b.n solver v_n;
        (* Link graph *)
        L.match_list_eq a.l b.l a.n b.n solver v_l
        |> L.match_ports_eq a.l b.l a.n b.n solver v_n v_l;
        S.simplify solver;
        match S.solve solver with UNSAT -> false | SAT -> true
      with
      | NOT_TOTAL -> false
      | NO_MATCH -> false)

  let equal a b =
    Big.(
      Link.Lg.cardinal a.l = Link.Lg.cardinal b.l
      && inter_equal (inner a) (inner b)
      && inter_equal (outer a) (outer b)
      && Place.size a.p = Place.size b.p
      && P.deg_regions a.p = P.deg_regions b.p
      && P.deg_sites a.p = P.deg_sites b.p
      && Nodes.equal a.n b.n
      && Sparse.equal a.p.Place.rs b.p.Place.rs
      &&
      (* Placing or wiring *)
      if Nodes.size b.n = 0 then
        Place.equal_placing a.p b.p && Link.Lg.equal a.l b.l
      else equal_SAT a b)

  (* Comparison over keys already performed and failed *)
  let equal_key a b =
    Big.(
      P.deg_regions a.p = P.deg_regions b.p
      && P.deg_sites a.p = P.deg_sites b.p
      && Sparse.equal a.p.Place.rs b.p.Place.rs
      &&
      (* Placing or wiring *)
      if Nodes.size b.n = 0 then
        Place.equal_placing a.p b.p && Link.Lg.equal a.l b.l
      else equal_SAT a b)
end