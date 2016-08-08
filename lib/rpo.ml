
type bound = Big.bg * Big.bg

type rpo = Big.bg * Big.bg * Big.bg

(* Sets of node identifiers {0, ..., size - 1} for a bound *)                 
let node_sets b =
  let aux b f = IntSet.of_int ((f b).Big.n.Nodes.size)
  in (aux b fst, aux b snd)

(* Give the root parent set of a node (either empty set or singleton) *)
let rootPrntN n b =
  let prntSet = Sparse.prn b.Big.p.Place.rn n in
  if IntSet.equal prntSet IntSet.empty then None
  else Some (IntSet.choose prntSet)

(* As before but for sites rather than nodes *)
let rootPrntS s b =
  let prntSet = Sparse.prn b.Big.p.Place.rs s in
  if IntSet.equal prntSet IntSet.empty then None
  else Some (IntSet.choose prntSet)

let rec rootEqui mHat tuple =
  match mHat with
  | hd :: tl -> if (TupleSet.mem tuple hd) then hd else (rootEqui tl tuple) 
  | [] -> TupleSet.empty

(* From Real World OCaml book, page 51. Drops value of list *)
let rec drop_value l to_drop =
  match l with
  | [] -> []
  | hd :: tl ->
    let new_tl = drop_value tl to_drop in
    if hd = to_drop then new_tl else hd :: new_tl

let joinSets (r0, r1) acc = (* Lines 18 to 28 of pseudo code *)
  let s = rootEqui acc (0,r0) in
  let t = rootEqui acc (1,r1) in
  let redAcc = drop_value (drop_value acc t) s in
  let newS = if (TupleSet.equal s TupleSet.empty) then TupleSet.singleton (0,r0) else s in
  let newT = if (TupleSet.equal t TupleSet.empty) then TupleSet.singleton (1,r1) else t in
  (TupleSet.union newS newT) :: redAcc
                 
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
  let h = IntSet.of_int ((fst a).Big.p.Place.s) in
  let p = IntSet.of_int ((fst d).Big.p.Place.r) in
  (* Now to the actual algorithm *)
  let vb0 = IntSet.diff va1 (Iso.codom i_a0_a1 |> IntSet.of_list) in  (* identifiers in a1 *)
  let vb1 = IntSet.diff va0 (Iso.dom i_a0_a1 |> IntSet.of_list) in (* identifiers in a0 *)
  let vb = IntSet.inter vd0 (Iso.dom i_d0_d1 |> IntSet.of_list) in (* identifiers in d0 *)
  (* Using a list of sets is easier than a set of sets. Start with the individual nodes *)
  let rInd0 = IntSet.fold (fun i acc -> 
                             match (rootPrntN i (fst a)) with
                             | None -> acc
                             | Some r -> IntSet.add r acc) 
                          vb1 IntSet.empty in
  let rInd1 = IntSet.fold (fun i acc -> 
                             match (rootPrntN i (snd a)) with
                             | None -> acc
                             | Some r -> IntSet.add r acc)
                          vb0 IntSet.empty in
  let mHatInd = IntSet.fold (fun i acc -> TupleSet.singleton (1,i) :: acc ) rInd1
                   (IntSet.fold (fun i acc -> TupleSet.singleton (0,i) :: acc ) rInd0 []) in
  let vShared = IntSet.inter va0 (Iso.dom i_a0_a1 |> IntSet.of_list) in (* Shared nodes -with ids from a1- *)
  let mHatSharedV = IntSet.fold (fun i acc ->
                                   match ((rootPrntN i (fst a), rootPrntN (Iso.apply_exn i_a0_a1 i) (snd a))) with
                                   | (Some r0, Some r1) -> joinSets (r0, r1) acc 
                                   | _ -> acc)
                                 vShared mHatInd in
  let mHatShared = IntSet.fold (fun i acc ->
                                   match ((rootPrntS i (fst a), rootPrntS i (snd a))) with
                                   | (Some r0, Some r1) -> joinSets (r0, r1) acc 
                                   | _ -> acc)
                    h mHatSharedV in
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
                
