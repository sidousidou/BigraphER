
type bound = Big.bg * Big.bg

type rpo = Big.bg * Big.bg * Big.bg

(* Sets of node identifiers {0, ..., size - 1} for a bound *)                 
let node_sets b =
  let aux b f = IntSet.of_int ((f b).Big.n.Nodes.size)
  in (aux b fst, aux b snd)

(* Give the root parent set of a node (either empty set or singleton) *)
let rootPrntN n b =
  Sparse.prn b.Big.p.Place.rn n
                 
(* RPO algorithm *)
(* The isomorphism are as follows:
   - i_a0_a1: a0 -> a1 
   - i_a0_d1: a0 -> d1 
   - i_d0_a1: d0 -> a1 
   - i_d0_d1: d0 -> d1
 *)
let rpo a d i_a0_a1 i_a0_d1 i_d0_a1 i_d0_d1 =
  let (va0, va1) = node_sets a in
  let (vd0, vd1) = node_sets d in
  (* Now to the actual algorithm *)
  let vb0 = IntSet.diff va1 (Iso.codom i_a0_a1 |> IntSet.of_list) in  (* identifiers in a1 *)
  let vb1 = IntSet.diff va0 (Iso.dom i_a0_a1 |> IntSet.of_list) in (* identifiers in a0 *)
  let vb = IntSet.inter vd0 (Iso.dom i_d0_d1 |> IntSet.of_list) in (* identifiers in d0 *)
  (* let mHat = [] in Using a list should be easier than a set of sets *)
  let r = IntSet.fold (fun i acc -> rootPrntN i (fst a) |> IntSet.union acc) vb1 IntSet.empty in
  let mHat = IntSet.fold (fun i acc -> TupleSet.singleton (0,i) :: acc ) r [] in
  (* Only dummy code for compilation *)
  (Big.id_eps, Big.id_eps, Big.id_eps)

(* Run tests here *)
let () =
  print_endline "Tests for RPO algorithm";
  let a0 = Big.id_eps 
  and a1 = Big.id_eps
  and d0 = Big.id_eps
  and d1 = Big.id_eps
  and b0 = Big.id_eps
  and b1 = Big.id_eps
  and b =  Big.id_eps
  and i_a = Iso.of_list_exn []
  and i_a' = Iso.of_list_exn []
  and i_d = Iso.of_list_exn []
  and i_d' = Iso.of_list_exn [] in
  assert (Big.equal (Big.comp d0 a0) (Big.comp d1 a1));
  assert (Big.equal (Big.comp b b0) d0);
  assert (Big.equal (Big.comp b b1) d1);
  let (b0', b1', b') = rpo (a0, a1) (d0, d1) i_a i_a' i_d i_d' in
  assert (Big.equal b0 b0');
  assert (Big.equal b1 b1');
  assert (Big.equal b b');
  print_endline "Test OK"
                
