open Printf
open Base

(* Type for concrete place graphs. The elements are regions, nodes, sites and
   for adjacency matrices: regions to nodes, regions to sites, nodes to nodes and
   nodes to sites. *)
type pg = { r: int;
            n: int;
            s: int;
            rn: Sparse.bmatrix;
            rs: Sparse.bmatrix;
            nn: Sparse.bmatrix;
            ns: Sparse.bmatrix;
          }

(* Raised by comp. The elements are (sites, regions) *)
exception COMP_ERROR of (int * int)

(* String representation *)
let to_string p =
  Sparse.stack (Sparse.append p.rn p.rs) (Sparse.append p.nn p.ns)
  |> Sparse.to_string
  |> fun s -> (string_of_int p.r) ^ " "
              ^ (string_of_int p.n) ^ " "
              ^ (string_of_int p.s) ^ "\n" ^ s

let json_of_place p =
  let open JSON in
  let m =
    Sparse.stack (Sparse.append p.rn p.rs) (Sparse.append p.nn p.ns) in
  Sparse.fold (fun i j acc ->
      let e = J_node [ J_int ("source", i); J_int ("target", j) ] in
      e :: acc) m []
  |> (fun m ->
      J_record ("place_graph", [ J_int ("regions", p.r);
                                 J_int ("nodes", p.n);
                                 J_int ("sites", p.s);
                                 J_array ("dag", m)
                               ]))

(* Parse a place graph from a list of strings *)
let parse ~regions:r ~nodes:n ~sites:s lines =
  assert (r >= 0);
  assert (n >= 0);
  assert (s >= 0);
  assert (List.length lines = r + n);
  assert (List.for_all (fun l -> String.length l = n + s) lines);
  let ((rn, rs), (nn, ns)) =
    Sparse.parse_string r n s lines in
  { r; n; s; rn; rs; nn; ns; }

(* Apply isomorphism *)
let apply_exn i p =
  { p with
    rn = Sparse.apply_cols_exn i p.rn;
    nn = Sparse.apply_exn i p.nn;
    ns = Sparse.apply_rows_exn i p.ns;
  }

(* Elementary place graphs *)
let elementary_id n =
  assert (n >= 0);
  { r = n;
    n = 0;
    s = n;
    rn = Sparse.make n 0;
    rs = Sparse.diag n;
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 n;
  }

let elementary_merge n =
  assert (n >= 0);
  { r = 1;
    n = 0;
    s = n;
    rn = Sparse.make 1 0;
    rs = Sparse.row n;
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 n;
  }

let elementary_split n =
  assert (n >= 0);
  { r = n;
    n = 0;
    s = 1;
    rn = Sparse.make n 0;
    rs = Sparse.col n;
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 1;
  }

let id0 = elementary_id 0

let one = elementary_merge 0

let zero = elementary_split 0

let elementary_sym m n =
  assert (m >= 0);
  assert (n >= 0);
  { r = m + n;
    n = 0;
    s = m + n;
    rn = Sparse.make (m + n) 0;
    rs = Sparse.stack
        (Sparse.append (Sparse.make n m) (Sparse.diag n))
        (Sparse.append (Sparse.diag m) (Sparse.make m n));
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 (m + n);
  }

let elementary_ion =
  { r = 1;
    n = 1;
    s = 1;
    rn = Sparse.row 1;
    rs = Sparse.make 1 1;
    nn = Sparse.make 1 1;
    ns = Sparse.row 1;
  }

(* Parse a placing *)
let parse_placing l r =
  assert (r >= 0);
  { r = r;
    n = 0;
    s = List.length l;
    rn = Sparse.make r 0;
    rs = Sparse.parse_vectors l r;
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 (List.length l);
  }

(* placing equality *)
let equal_placing a b =
  assert (a.n = 0);
  assert (b.n = 0);
  (a.r = b.r) && (a.s = b.s) && (Sparse.equal a.rs b.rs)

(* placing compare *)
let compare_placing a b =
  assert (a.n = 0);
  assert (b.n = 0);
  match a.r - b.r with
  | 0 ->
    (match a.s - b.s with
     | 0 -> Sparse.compare a.rs b.rs
     | x -> x)
  | x -> x

(* Tensor product: A x B (indices in the right hand-side are increased) *)
let tens a b =
  { r = a.r + b.r;
    n = a.n + b.n;
    s = a.s + b.s;
    rn = Sparse.tens a.rn b.rn;
    rs = Sparse.tens a.rs b.rs;
    nn = Sparse.tens a.nn b.nn;
    ns = Sparse.tens a.ns b.ns;
  }

let tens_of_list =
  List.fold_left (fun acc a ->
      tens acc a)
    id0

(* Composition: G o F (indices in the right hand-side are increased) *)
let comp g f =
  if g.s = f.r then
    { r = g.r;
      n = g.n + f.n;
      s = f.s;
      rn = Sparse.append g.rn (Sparse.mul g.rs f.rn);
      rs = Sparse.mul g.rs f.rs;
      nn = Sparse.stack
          (Sparse.append g.nn (Sparse.mul g.ns f.rn))
          (Sparse.append (Sparse.make f.n g.n) f.nn);
      ns = Sparse.stack (Sparse.mul g.ns f.rs) f.ns;
    }
  else raise (COMP_ERROR (g.s, f.r))

(* Is p an identity? *)
let is_id = function
  | { r = x; n = 0; _ } as p -> p.rs = Sparse.diag x
  | _ -> false

let is_plc p =
  p.n = 0

let is_ground p =
  p.s = 0

(* Nodes with no children (both nodes and sites children). *)
let leaves p =
  Sparse.glue p.rn p.rs p.nn p.ns
  |> Sparse.leaves  (* regions and nodes *)
  |> IntSet.off (-p.r)
  |> IntSet.filter (fun x -> x >= 0) (* only nodes *)

(* Dual *)
let orphans p =
  Sparse.glue p.rn p.rs p.nn p.ns
  |> Sparse.orphans  (* nodes and sites *)
  |> IntSet.filter (fun x -> x < p.n) (* only nodes *)

(* Is p monomorphic?: no two sites are siblings and no site is an orphan *)
let is_mono p =
  let m = Sparse.glue p.rn p.rs p.nn p.ns in
  if Sparse.orphans m
     |> IntSet.filter (fun x -> x >= p.n) (* only sites *)
     |> IntSet.is_empty
  then IntSet.of_int p.s
       |> IntSet.off p.n
       |> IntSet.for_all (fun j ->
           Sparse.siblings m j
           |> IntSet.filter (fun x -> x >= p.n) (* only sites *)
           |> IntSet.is_empty)
  else false

(* Is p epimorphic: no region is idle and no two regions are partners *)
let is_epi p =
  let m = Sparse.glue p.rn p.rs p.nn p.ns in
  if Sparse.leaves m
     |> IntSet.filter (fun x -> x < p.r) (* only regions *)
     |> IntSet.is_empty
  then IntSet.of_int p.r
       |> IntSet.for_all (fun i ->
           Sparse.partners m i
           |> IntSet.filter (fun x -> x < p.r) (* only regions *)
           |> IntSet.is_empty)
  else false

(* Is p guarded: no region has sites as children *)
let is_guard p =
  (Sparse.entries p.rs) = 0

(* Build the decomposition of target t given pattern p and isomorphism over
   nodes i: p -> t. The result is context c, id, d, and nodes in c and d
   expressed as rows of t. Pattern p is mono and epi.
   See page 76, proposition 4.2.4. *)
let decomp ~target:t ~pattern:p iso =
  let trans_t_nn = Sparse.trans t.nn (* memoisation *)
  and iso' = Iso.inverse iso
  and v_p' = IntSet.of_list (Iso.codom iso) in
  (* ancestors of v_p' not in v_p' *)
  let v_c =
    IntSet.diff
      (IntSet.fold (fun i acc ->
           Sparse.prn trans_t_nn i
           |> IntSet.union acc)
          v_p' IntSet.empty)
      v_p' in
  (* all the other nodes *)
  let v_d = IntSet.diff
      (IntSet.of_int t.n)
      (IntSet.union v_c v_p') in
  (* fix numbering of nodes in c and d : t -> c and t -> d *)
  let iso_v_c = IntSet.fix v_c
  and iso_v_d = IntSet.fix v_d
  (* IntSet of target's regions *)
  and tr_set = IntSet.of_int t.r in
  (************************** Identity **************************)
  (* c regions to d nodes *)
  let (edg_c_rs0, edg_d_rn0, s0) =
    IntSet.fold (fun r acc ->
        IntSet.fold
          (fun c (acc_c, acc_d, j)  ->
             if IntSet.mem c v_d then
               ((r, j + p.r) :: acc_c,
                (j + p.s, safe (Iso.apply iso_v_d c)) :: acc_d,
                j + 1)
             else (acc_c, acc_d, j))
          (Sparse.chl t.rn r) acc)
      tr_set ([], [], 0) in
  (* c regions to d sites *)
  let (edg_c_rs1, edg_d_rs0, s1) =
    IntSet.fold (fun r acc ->
        IntSet.fold (fun c (acc_c, acc_d, s) ->
            ((r, s + p.r + s0) :: acc_c,
             (s + p.s + s0, c) :: acc_d,
             s + 1))
          (Sparse.chl t.rs r) acc)
      tr_set ([], [], 0) in
  (* c nodes to d nodes *)
  let (edg_c_ns0, edg_d_rn1, s2) =
    IntSet.fold (fun i acc ->
        IntSet.fold (fun c (acc_c, acc_d, j) ->
            if IntSet.mem c v_d then
              ((safe (Iso.apply iso_v_c i), j + p.r + s0 + s1) :: acc_c,
               (j + p.s + s0 + s1, safe (Iso.apply iso_v_d c)) :: acc_d,
               j + 1)
            else (acc_c, acc_d, j))
          (Sparse.chl t.nn i) acc)
      v_c ([], [], 0) in
  (* c nodes to d sites *)
  let (edg_c_ns1, edg_d_rs1, s3) =
    IntSet.fold (fun i acc ->
        IntSet.fold (fun c (acc_c, acc_d, s) ->
            ((safe (Iso.apply iso_v_c i), s + p.r + s0 + s1 + s2) :: acc_c,
             (s + p.s + s0 + s1 + s2, c) :: acc_d,
             s + 1))
          (Sparse.chl t.ns i) acc)
      v_c ([], [], 0)
  (************************** Context **************************)
  (* c regions to p nodes *)
  and edg_c_rp =
    Sparse.fold_r (fun r js acc ->
        let sites = IntSet.filter_apply js iso'
                    |> Sparse.row_eq p.rn in
        IntSet.fold (fun s acc -> (r, s) :: acc) sites acc)
      t.rn []
  (* c nodes to p nodes *)
  and edg_c_np =
    IntSet.fold (fun r acc ->
        let sites =
          IntSet.filter_apply (Sparse.chl t.nn r) iso'
          |> Sparse.row_eq p.rn in
        IntSet.fold (fun s acc ->
            (safe (Iso.apply iso_v_c r), s) :: acc)
          sites acc)
      v_c []
  (************************** Parameter **************************)
  (* p nodes to d nodes *)
  and edg_d_nn =
    IntSet.fold (fun n acc ->
        let sites =
          IntSet.filter_apply (Sparse.prn t.nn n) iso'
          |> Sparse.col_eq p.ns in
        IntSet.fold (fun s acc ->
            (s, safe (Iso.apply iso_v_d n)) :: acc)
          sites acc)
      v_d []
  (* p nodes to d sites *)
  and edg_d_ns =
    Sparse.fold_c (fun s is acc ->
        let sites = IntSet.filter_apply is iso'
                    |> Sparse.col_eq p.ns in
        IntSet.fold (fun r acc -> (r, s) :: acc) sites acc)
      t.ns [] in
  (* size of id *)
  let j = s0 + s1 + s2 + s3 in
  (* Context c *)
  let c =
    let n = IntSet.cardinal v_c
    and s = p.r + j in
    { r = t.r;
      n = n;
      s = s;
      rn = Sparse.fold (fun i j acc ->
          if IntSet.mem j v_c
          then Sparse.add i (safe (Iso.apply iso_v_c j)) acc
          else acc)
          t.rn (Sparse.make t.r n);
      rs = Sparse.add_list
          (Sparse.make t.r s)
          (edg_c_rs0 @ edg_c_rs1 @ edg_c_rp);
      nn = Sparse.fold (fun i j acc ->
          if (IntSet.mem i v_c) && (IntSet.mem j v_c)
          then Sparse.add (safe (Iso.apply iso_v_c i))
              (safe (Iso.apply iso_v_c j))
              acc
          else acc)
          t.nn (Sparse.make n n);
      ns = Sparse.add_list
          (Sparse.make n s)
          (edg_c_ns0 @ edg_c_ns1 @ edg_c_np);
    }
  (* Parameter d *)
  and d =
    let n = IntSet.cardinal v_d
    and r = p.s + j in
    { r = r;
      n = n;
      s = t.s;
      rn = Sparse.add_list
          (Sparse.make r n)
          (edg_d_rn0 @ edg_d_rn1 @ edg_d_nn);
      rs = Sparse.add_list
          (Sparse.make r t.s)
          (edg_d_rs0 @ edg_d_rs1 @ edg_d_ns);
      nn = Sparse.fold (fun i j acc ->
          if (IntSet.mem i v_d) && (IntSet.mem j v_d)
          then Sparse.add (safe (Iso.apply iso_v_d i))
              (safe (Iso.apply iso_v_d j))
              acc
          else acc)
          t.nn (Sparse.make n n);
      ns = Sparse.fold (fun i j acc ->
          if IntSet.mem i v_d
          then Sparse.add (safe (Iso.apply iso_v_d i)) j acc
          else acc)
          t.ns (Sparse.make n t.s);
    } in
  (c, d, elementary_id j, iso_v_c, iso_v_d)

(* Compute three strings to build a dot representation.*)
let get_dot p =
  (* Attributes for regions and sits *)
  let attr =
    "shape=box, width=.28, height=.18, fontname=\"serif\", fontsize=9.0"
  (* Graph edges *)
  and arr s d i j buff =
    sprintf "%s%c%d -> %c%d [ arrowhead=\"vee\", arrowsize=0.5 ];\n"
      buff s i d j in
  (* Region shapes *)
  let region_shapes =
    IntSet.fold (fun i buff ->
        sprintf "%sr%d [ label=\"%d\", style=\"dashed\", %s ];\n"
          buff i i attr)
      (IntSet.of_int p.r) ""
  (* Site shapes *)
  and site_shapes =
    IntSet.fold (fun i buff ->
        sprintf "%ss%d [ \
                 label=\"%d\", \
                 style=\"filled,dashed\", \
                 fillcolor=\"gray\", \
                 %s ];\n" buff i i attr)
      (IntSet.of_int p.s) ""
  (* Ranks *)
  and ranks =
    List.fold_left (fun buff ns ->
        sprintf "%s{ rank=same; %s };\n" buff
          (IntSet.fold (fun i acc ->
               (sprintf "v%d" i) :: acc) ns []
           |> String.concat "; " ))
      "" (Sparse.levels p.nn)
  (* Adjacency matrix *)
  and m_rn =
    Sparse.fold (arr 'r' 'v') p.rn ""
  and m_rs =
    Sparse.fold (arr 'r' 's') p.rs ""
  and m_nn =
    Sparse.fold (arr 'v' 'v') p.nn ""
  and m_ns =
    Sparse.fold (arr 'v' 's') p.ns "" in
  (region_shapes, site_shapes, ranks, String.concat "" [m_rn; m_rs; m_nn; m_ns])

(* Number of edges in the DAG *)
let edges p =
  (Sparse.entries p.rn) + (Sparse.entries p.rs)
  + (Sparse.entries p.nn) + (Sparse.entries p.ns)

module H =
  Hashtbl.Make(struct
    type t = string * string
    let equal (x:string * string) y = x = y
    let hash = Hashtbl.hash
  end)

(* given an edge of control A -> B, find all the edges with the same type.
   return a hash table (string * string) -> (int * int) *)
let partition_edges p n =
  let h = H.create (Sparse.entries p.nn) in
  Sparse.iter (fun i j ->
      match (Nodes.get_ctrl_exn i n, Nodes.get_ctrl_exn j n) with
      | (Ctrl.C(a_string, _), Ctrl.C(b_string, _)) ->
        H.add h (a_string, b_string) (i, j))
    p.nn;
  h

type deg =
  | V of int (* only vertices *)
  | S of int (* with sites or regions *)

let indeg p i =
  assert (i >= 0);
  let d = IntSet.cardinal (Sparse.prn p.nn i) in
  if IntSet.is_empty (Sparse.prn p.rn i) then V d
  else S d

let outdeg p i =
  assert (i >= 0);
  let d = IntSet.cardinal (Sparse.chl p.nn i) in
  if IntSet.is_empty (Sparse.chl p.ns i) then V d
  else S d

(* true if the degrees are compatible *)
let compat_deg t p =
  match p with
  | V d ->
    (match t with
     | V d' -> d = d'
     | S _ -> false)
  | S d ->
    (match t with
     | V d' -> d' >= d
     | S d' -> d' >= d)

let compat t p t_i p_i =
  (compat_deg (indeg t t_i) (indeg p p_i))
  && (compat_deg (outdeg t t_i) (outdeg p p_i))

let eq t p t_i p_i =
  ((indeg t t_i) = (indeg p p_i))
  && ((outdeg t t_i) = (outdeg p p_i))

exception NOT_TOTAL

(* Match nodes in compatible DAG edges *)
let match_list ~target:t ~pattern:p ~n_t ~n_p =
  let h = partition_edges t n_t in
  let (clauses, clauses_exc, cols) =
    Sparse.fold (fun i j (acc, exc, acc_c) ->
        let (a, b) =
          (Nodes.get_ctrl_exn i n_p, Nodes.get_ctrl_exn j n_p) in
        match (a, b) with
        | (Ctrl.C(a_string, _), Ctrl.C(b_string, _)) ->
          begin
            let t_edges =
              List.filter
                (fun (i', j') ->
                   (* Degree check *)
                   (compat t p i' i) && (compat t p j' j))
                (H.find_all h (a_string, b_string)) in
            match t_edges with
            | [] -> (* No compatible edges found *)
              raise_notrace NOT_TOTAL
            | t_edges ->
              begin
                let new_c =
                  List.fold_left (fun acc (i', j') ->
                      i' :: j' :: acc)
                    [] t_edges in
                match
                  (List.map (fun (i', j') ->
                       (Cnf.M_lit (i, i'), Cnf.M_lit (j, j')))
                      t_edges)
                  |> Cnf.tseitin
                with
                | Cnf.Conj clauses ->
                  (acc, clauses @ exc, new_c @ acc_c)
                | Cnf.Enc (clause, pairs) ->
                  ((clause, pairs) :: acc, exc, new_c @ acc_c)
              end
          end)
      p.nn ([], [], []) in
  (clauses, clauses_exc, IntSet.of_list cols) (* matched columns *)

(* leaves (orphans) in p are matched to leaves (orphans) in t.
   C5: ij0 or ij1 or ..... *)
let match_leaves ~target:t ~pattern:p ~n_t ~n_p =
  let l_p = leaves p
  and l_t = leaves t in
  let (clauses, c) =
    IntSet.fold (fun i (acc, acc_c) ->
        let c = Nodes.get_ctrl_exn i n_p in
        let compat_t =
          IntSet.inter (Nodes.find_all c n_t) l_t in
        if IntSet.is_empty compat_t then
          raise_notrace NOT_TOTAL
        else begin
          ((IntSet.fold (fun j acc ->
               Cnf.P_var (Cnf.M_lit (i, j)) :: acc
             ) compat_t []) :: acc,
           IntSet.union acc_c compat_t)
        end)
      l_p ([], IntSet.empty) in
  (clauses, c)

(* Dual *)
let match_orphans ~target:t ~pattern:p ~n_t ~n_p =
  let o_p = orphans p
  and o_t = orphans t in
  let (clauses, c) =
    IntSet.fold (fun i (acc, acc_c) ->
        let c = Nodes.get_ctrl_exn i n_p in
        let compat_t =
          IntSet.inter (Nodes.find_all c n_t) o_t in
        if IntSet.is_empty compat_t then
          raise_notrace NOT_TOTAL
        else begin
          ((IntSet.fold (fun j acc ->
               Cnf.P_var (Cnf.M_lit (i, j)) :: acc)
               compat_t []) :: acc,
           IntSet.union acc_c compat_t)
        end)
      o_p ([], IntSet.empty) in
  (clauses, c)

(* Only ctrl and deg check *)
let match_ctrl_deg_aux t p n_t n_p m =
  Sparse.fold (fun i _ (acc, acc_c) ->
      let js =
        Nodes.get_ctrl_exn i n_p
        |> flip Nodes.find_all n_t
        |> IntSet.filter (fun j -> compat t p j i) in
      if IntSet.is_empty js then raise_notrace NOT_TOTAL
      else let clause =
             IntSet.fold (fun j acc ->
                 (Cnf.P_var (Cnf.M_lit (i, j))) :: acc)
               js [] in
        (clause :: acc, IntSet.union js acc_c))
    m ([], IntSet.empty)

let match_sites ~target ~pattern ~n_t ~n_p =
  match_ctrl_deg_aux target pattern n_t n_p pattern.ns

let match_regions ~target ~pattern ~n_t ~n_p =
  match_ctrl_deg_aux target pattern n_t n_p pattern.rn

(* Block unconnected pairs of nodes with sites and nodes with regions. *)
let match_trans ~target:t ~pattern:p : Cnf.clause list =
  let n_s = Sparse.dom p.ns (* index i *)
  and n_r = Sparse.codom p.rn in (* index j *)
  let (n_s', n_r') =
    IntSet.fold (fun i (acc_s, acc_r) ->
        let x =
          IntSet.inter (Sparse.chl p.nn i) n_r in
        if IntSet.cardinal x = 0 then (acc_s, acc_r)
        else (IntSet.remove i acc_s, IntSet.diff acc_r x))
      n_s (n_s, n_r) in
  (* For every edge (i', j') in t, block any ii' and jj' *)
  Sparse.fold (fun i' j' acc ->
      let blocks = IntSet.fold (fun i acc ->
          IntSet.fold (fun j acc ->
              [ Cnf.N_var (Cnf.M_lit (i, i'));
                Cnf.N_var (Cnf.M_lit (j, j')) ] :: acc)
            n_r' acc)
          n_s' [] in
      blocks @ acc)
    t.nn []

let check_sites t p v_p' c_set iso =
  let s_set =
    IntSet.fold (fun j acc ->
        let children = Sparse.chl t.ns j in
        IntSet.union acc children) v_p' IntSet.empty in
  (* Is there a set of sites with the same parent set? *)
  (* Nodes *)
  (IntSet.for_all (fun c ->
       let prn_c =
         IntSet.inter v_p' (Sparse.prn t.nn c) in
       (* Construct a candidate set of sites *)
       let candidate =
         IntSet.fold (fun s acc ->
             let prn_s =
               safe_exn (IntSet.apply_exn (Sparse.prn p.ns s) iso) in
             if IntSet.subset prn_s prn_c then IntSet.union prn_s acc
             else acc)
           (IntSet.of_int p.s) IntSet.empty in
       (* Equality test *)
       IntSet.equal candidate prn_c)
      c_set) &&
  (* Sites *)
  (IntSet.for_all (fun s ->
       let prn_s =
         IntSet.inter v_p' (Sparse.prn t.ns s) in
       (* Construct a candidate set of sites *)
       let candidate =
         IntSet.fold (fun s acc ->
             let prn_s' =
               safe_exn (IntSet.apply_exn (Sparse.prn p.ns s) iso) in
             if IntSet.subset prn_s' prn_s then IntSet.union prn_s' acc
             else acc)
           (IntSet.of_int p.s) IntSet.empty in
       (* Equality test *)
       IntSet.equal candidate prn_s)
      s_set)

(* Dual *)
let check_regions t p v_p' iso =
  let p_set =
    IntSet.fold (fun j acc ->
        let parents =
          IntSet.diff (Sparse.prn t.nn j) v_p' in
        IntSet.union acc parents) v_p' IntSet.empty
  and r_set =
    IntSet.fold (fun j acc ->
        let parents = Sparse.prn t.rn j in
        IntSet.union acc parents) v_p' IntSet.empty in
  (* Is there a set of regions with the same children set? *)
  (* Nodes *)
  (IntSet.for_all (fun x ->
       let chl_p =
         IntSet.inter v_p' (Sparse.chl t.nn x) in
       (* Construct a candidate set of regions *)
       let candidate =
         IntSet.fold (fun r acc ->
             let chl_r =
               safe_exn (IntSet.apply_exn (Sparse.chl p.rn r) iso) in
             if IntSet.subset chl_r chl_p then IntSet.union chl_r acc
             else acc
           ) (IntSet.of_int p.r) IntSet.empty in
       (* Equality test *)
       IntSet.equal candidate chl_p
     ) p_set) &&
  (* Regions *)
  (IntSet.for_all (fun x ->
       let chl_r =
         IntSet.inter v_p' (Sparse.chl t.rn x) in
       (* Construct a candidate set of regions *)
       let candidate =
         IntSet.fold (fun r acc ->
             let chl_r' =
               safe_exn (IntSet.apply_exn (Sparse.chl p.rn r) iso) in
             if IntSet.subset chl_r' chl_r then IntSet.union chl_r' acc
             else acc
           ) (IntSet.of_int p.r) IntSet.empty in
       (* Equality test *)
       IntSet.equal candidate chl_r
     ) r_set)

(* check TRANS *)
let check_trans t_trans v_p' c_set =
  (* check if there is a node child of co-domain, outside co-domain, such that
     one of its children in trans is in co-domain *)
  not (IntSet.exists (fun c ->
      IntSet.exists (fun t ->
          IntSet.mem t v_p')
        (Sparse.chl t_trans c))
      c_set)

(* Check if iso i : p -> t is valid *)
let check_match ~target:t ~pattern:p t_trans iso =
  let v_p' =
    IntSet.of_list (Iso.codom iso) in
  let c_set =
    IntSet.fold (fun j acc ->
        IntSet.diff (Sparse.chl t.nn j) v_p'
        |> IntSet.union acc)
      v_p' IntSet.empty in
  (check_sites t p v_p' c_set iso)
  && (check_regions t p v_p' iso)
  && (check_trans t_trans v_p' c_set)

(* ++++++++++++++++++++++ Equality functions ++++++++++++++++++++++ *)

let deg_regions p =
  IntSet.fold (fun r acc ->
      (IntSet.cardinal (Sparse.chl p.rn r)) :: acc)
    (IntSet.of_int p.r) []

let deg_sites p =
  IntSet.fold (fun s acc ->
      (IntSet.cardinal (Sparse.prn p.ns s)) :: acc)
    (IntSet.of_int p.s) []

let match_list_eq p t n_p n_t =
  let h = partition_edges t n_t in
  let (clauses, clauses_exc, cols) =
    Sparse.fold (fun i j (acc, exc, acc_c) ->
        let (a, b) =
          (Nodes.get_ctrl_exn i n_p, Nodes.get_ctrl_exn j n_p) in
        match (a, b) with
        | (Ctrl.C (a_string, _), Ctrl.C (b_string, _)) ->
          begin
            let t_edges =
              List.filter
                (fun (i', j') ->
                   (* Degree equality *)
                   (eq t p i' i) && (eq t p j' j))
                (H.find_all h (a_string, b_string)) in
            match t_edges with
            | [] -> (* No compatible edges found *)
              raise_notrace NOT_TOTAL
            | t_edges ->
              begin
                match
                  (List.map (fun (i', j') ->
                       (Cnf.M_lit (i, i'), Cnf.M_lit (j, j')))
                      t_edges)
                  |> Cnf.tseitin
                with
                | Cnf.Enc (clause, pairs) ->
                  ((clause, pairs) :: acc, exc, acc_c)
                | Cnf.Conj clauses ->
                  (acc, clauses @ exc, acc_c)
              end
          end)
      p.nn ([], [], []) in
  (clauses, clauses_exc, IntSet.of_list cols) (* matched columns *)

(* out clauses = (ij1 or ij2 or ij ...) :: ... *)
let match_region_nodes a b n_a n_b =
  Sparse.fold (fun r i (acc, acc_c) ->
      let c = Nodes.get_ctrl_exn i n_a in
      let children =
        IntSet.filter (fun i ->
            Ctrl.(=) c (Nodes.get_ctrl_exn i n_b))
          (Sparse.chl b.rn r) in
      ((IntSet.fold (fun j acc ->
           (Cnf.P_var (Cnf.M_lit (i, j))) :: acc)
           children []) :: acc,
       (*IntSet.union acc_c (IntSet.of_list children)*)
       acc_c))
    a.rn ([], IntSet.empty)

(*Dual*)
let match_nodes_sites a b n_a n_b =
  Sparse.fold (fun i s (acc, acc_c) ->
      let c = Nodes.get_ctrl_exn i n_a in
      let parents =
        IntSet.filter (fun i ->
            Ctrl.(=) c (Nodes.get_ctrl_exn i n_b))
          (Sparse.prn b.ns s) in
      ((IntSet.fold (fun j acc ->
           (Cnf.P_var (Cnf.M_lit (i, j))) :: acc)
           parents []) :: acc,
       (* IntSet.union acc_c (IntSet.of_list parents) *)
       acc_c))
    a.ns ([], IntSet.empty)

(******************************************************************************)
(* Compute the reachable set via Depth First Search. *)
exception NOT_PRIME

let rec dfs_ns p l res_n marked_n =
  match l with
  | [] -> res_n
  | i :: l' ->
    let js = Sparse.chl p.nn i in
    if IntSet.disjoint marked_n js then
      let js' = IntSet.diff js res_n in
      dfs_ns p ((IntSet.elements js') @ l') (IntSet.union js' res_n) marked_n
    else raise NOT_PRIME

let dfs_r p r marked_n =
  let js = Sparse.chl p.rn r in
  if IntSet.disjoint js marked_n then
    dfs_ns p (IntSet.elements js) js marked_n
  else raise NOT_PRIME

let dfs p =
  (* Only for ground place graphs *)
  assert (p.s = 0);
  let rec aux i res marked_n =
    match i with
    | 0 ->
      let res_n = dfs_r p 0 marked_n in
      (res_n :: res, IntSet.union res_n marked_n)
    | -1 -> (res, marked_n)
    | _ ->
      let res_n = dfs_r p i marked_n in
      aux (i - 1)
        (res_n :: res)
        (IntSet.union res_n marked_n) in
  aux (p.r - 1) [] IntSet.empty

let build_comp_aux p p' nodes iso =
  { p' with nn = IntSet.fold (fun i acc ->
        let js = Sparse.chl p.nn i
        and i' = safe (Iso.apply iso i) in
        IntSet.fold (fun j acc ->
            Sparse.add i' (safe (Iso.apply iso j)) acc)
          js acc)
        nodes p'.nn;
  }

let rec chl_of_regions d acc stop i =
  if i < stop then acc
  else let acc' =
         Sparse.chl d.rn i
         |> IntSet.union acc in
    chl_of_regions d acc' stop (i - 1)

let build_d p first last nodes =
  let n = IntSet.cardinal nodes
  and r = last - first + 1
  and iso = IntSet.fix nodes in
  let region_set = IntSet.of_int r
                 |> IntSet.off first in
  let iso_regions = IntSet.fix region_set in
  let p' = { r = r;
             n = n;
             s = 0;
             rn = IntSet.fold (fun r acc ->
                 let r' = safe (Iso.apply iso_regions r) in
                 IntSet.fold (fun j acc ->
                     Sparse.add r' (safe (Iso.apply iso j)) acc)
                   (Sparse.chl p.rn r) acc)
                 region_set (Sparse.make r n);
             rs = Sparse.make r 0;
             nn = Sparse.make n n;
             ns = Sparse.make n 0;
           } in
  (build_comp_aux p p' nodes iso, iso)

(* Build a prime component P' starting from P, a region and a set of nodes. An
   isomorphism P -> P' is also generated. *)
let build_component p r nodes =
  build_d p r r nodes

let build_o_component p nodes =
  build_d p 0 (-1) nodes

(* Sub-graph regioned in the orphan nodes of p. *)
let orphan_component p marked_n =
  (* let o_set = IntSet.diff (Sparse.orphans p.nn) (Sparse.codom p.rn) in *)
  let o_set = orphans p in
  dfs_ns p (IntSet.elements o_set) o_set marked_n

(* Return a list of bigraphs *)
let prime_components p =
  let (comps, marked_n) = dfs p in
  let o_nodes = orphan_component p marked_n in
  (List.mapi (fun r nodes -> build_component p r nodes) comps) @
  [build_o_component p o_nodes]

let decomp_d d id_n =
  let js_set = chl_of_regions d IntSet.empty 0 (d.r - id_n - 1) in
  let js = IntSet.elements js_set in
  let d'_nodes = dfs_ns d js js_set IntSet.empty in
  let id_set = chl_of_regions d IntSet.empty (d.r - id_n) (d.r - 1) in
  let ids = IntSet.elements id_set in
  let id_nodes = IntSet.union
      (dfs_ns d ids id_set d'_nodes)
      (orphan_component d d'_nodes) in
  let (d', iso_d') = build_d d 0 (d.r - id_n - 1) d'_nodes
  and (id, iso_id) = build_d d (d.r - id_n) (d.r - 1) id_nodes in
  (d', id, iso_d', iso_id)

(******************************************************************************)
