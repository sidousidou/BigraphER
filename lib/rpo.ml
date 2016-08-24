
type bound = Big.bg * Big.bg

type rpo = bound * Big.bg

type rpo_match = int Iso.t * int Iso.t * int Iso.t * int Iso.t
                               
exception NOT_RESIDENT

(* Sets of node identifiers {0, ..., size - 1} for a bound *)                 
let node_sets b =
  let aux b f = IntSet.of_int ((f b).Big.n.Nodes.size)
  in (aux b fst, aux b snd)

(* Give the root parent set of a node (either empty set or singleton) *)
let rootPrntN n b red =
  let prntSet = Sparse.prn b.Big.p.Place.rn n in
  if IntSet.equal prntSet IntSet.empty then None
  else if IntSet.subset prntSet red then None 
  else Some (IntSet.choose prntSet)

(* As before but for sites rather than nodes *)
let rootPrntS s b red =
  let prntSet = Sparse.prn b.Big.p.Place.rs s in
  if IntSet.equal prntSet IntSet.empty then None
  else if IntSet.subset prntSet red then None
  else Some (IntSet.choose prntSet)

let rec rootEqui mHat tuple =
  match mHat with
  | hd :: tl -> if (TupleSet.mem tuple hd) then hd 
                else (rootEqui tl tuple) 
  | [] -> TupleSet.empty

 
let rec _buildRed n s a0 a1 red0 red1 iso =
  if IntSet.is_empty n then (* Nodes or sides? *)
    (if IntSet.is_empty s then (red0, red1) (* Base case *)
    else
      let w = IntSet.choose s in
      match (rootPrntS w a0 red0) with
      | Some r0 ->
        (match (rootPrntS w a1 red1) with
        | Some _ -> _buildRed n (IntSet.remove w s) a0 a1 red0 red1 iso
        | None -> buildRed a0 a1 (IntSet.add r0 red0) red1 iso)
      | None ->
        (match (rootPrntS w a1 red1) with
        | Some r1 -> buildRed a0 a1 red0 (IntSet.add r1 red1) iso
        | None -> _buildRed n (IntSet.remove w s) a0 a1 red0 red1 iso))
  else
    let w = IntSet.choose n in
    match (rootPrntN w a0 red0) with
    | Some r0 ->
      (match (rootPrntN (Iso.apply_exn iso w) a1 red1) with
      | Some _ -> _buildRed (IntSet.remove w n) s a0 a1 red0 red1 iso
      | None -> buildRed a0 a1 (IntSet.add r0 red0) red1 iso)
    | None ->
      (match (rootPrntN (Iso.apply_exn iso w) a1 red1) with
      | Some r1 -> buildRed a0 a1 red0 (IntSet.add r1 red1) iso
      | None -> _buildRed (IntSet.remove w n) s a0 a1 red0 red1 iso)
and buildRed a0 a1 red0 red1 iso = 
  _buildRed (Iso.dom iso |> IntSet.of_list) (a0.Big.p.Place.s |> IntSet.of_int) a0 a1 red0 red1 iso  
 
(* From Real World OCaml book, page 51. Drops value of list *)
let rec drop_value l to_drop =
  match l with
  | [] -> []
  | hd :: tl ->
    let new_tl = drop_value tl to_drop in
    if hd = to_drop then new_tl else hd :: new_tl

let joinSets (r0, r1) acc = (* Lines 18 to 22 of pseudo code *)
  let s = rootEqui acc (0,r0) in
  let t = rootEqui acc (1,r1) in
  let redAcc = drop_value (drop_value acc t) s in
  (TupleSet.union s t) :: redAcc

(* Returns the integer position in which the set resides *)
let rec listPos ?(acc=0) l set =
  match l with
  | hd :: tl -> if (TupleSet.equal set hd) then acc 
                else listPos ~acc:(acc+1) tl set
  | [] -> raise NOT_RESIDENT
  
                 
(* RPO algorithm *)
(* The isomorphism are as follows:
   - i_a0_a1: a0 -> a1 
   - i_a0_d1: a0 -> d1 
   - i_d0_a1: d0 -> a1 
   - i_d0_d1: d0 -> d1
 *)
let rpo a d (i_a0_a1, i_a0_d1, i_d0_a1, i_d0_d1) =
  assert (Big.is_epi (fst a));
  assert (Big.is_epi (snd a));
  assert (Big.is_epi (fst d));
  assert (Big.is_epi (snd d));
  let (va0, va1) = node_sets a in
  let (vd0, _) = node_sets d in
  let h = IntSet.of_int ((fst a).Big.p.Place.s) in
  let p = IntSet.of_int ((fst d).Big.p.Place.r) in
  let m0 = IntSet.of_int ((fst a).Big.p.Place.r) in
  let m1 = IntSet.of_int ((snd a).Big.p.Place.r) in
  (* Now to the actual algorithm *)
  let vb0 = IntSet.diff va1 (Iso.codom i_a0_a1 |> IntSet.of_list) in  (* identifiers in a1 *)
  let vb1 = IntSet.diff va0 (Iso.dom i_a0_a1 |> IntSet.of_list) in (* identifiers in a0 *)
  let vb = IntSet.inter vd0 (Iso.dom i_d0_d1 |> IntSet.of_list) in (* identifiers in d0 *)
  (* Using a list of sets is easier than a set of sets. Start with the individual nodes *)
  let (red0, red1) = buildRed (fst a) (snd a) IntSet.empty IntSet.empty i_a0_a1 in
  let mHatInd =
       IntSet.fold
          (fun i acc ->
             if IntSet.mem i red0 then acc
             else TupleSet.singleton (0,i) :: acc)
          ((fst a).Big.p.Place.r |> IntSet.of_int)
          (IntSet.fold
             (fun i acc ->
                if IntSet.mem i red1 then acc
                else TupleSet.singleton (1,i) :: acc)
             ((snd a).Big.p.Place.r |> IntSet.of_int)
             []) in
  (* Shared nodes -with ids from a1- *)
  let vShared = IntSet.inter va0 (Iso.dom i_a0_a1 |> IntSet.of_list) in
  let mHatShared = 
       IntSet.fold 
          (fun i acc ->
             match ((rootPrntN i (fst a) red0, 
                     rootPrntN (Iso.apply_exn i_a0_a1 i) (snd a) red1)) with
             | (Some r0, Some r1) -> joinSets (r0, r1) acc 
             | _ -> acc)
          vShared 
          mHatInd in
  let mHat = 
       IntSet.fold 
          (fun i acc ->
             match ((rootPrntS i (fst a) red0, rootPrntS i (snd a) red1)) with
             | (Some r0, Some r1) -> joinSets (r0, r1) acc 
             | _ -> acc)
          h 
          mHatShared in
  (* Second page Pseudo Code *)
  let b0RS = 
       IntSet.fold 
          (fun i acc ->
             let prnt = rootEqui mHat (0,i) in
             if TupleSet.equal TupleSet.empty prnt then acc
             else Sparse.add (listPos mHat prnt) i acc) 
          m0 
          (Sparse.make (List.length mHat) (IntSet.cardinal m0)) in 
  let b1RS = 
       IntSet.fold 
          (fun i acc ->
             let prnt = rootEqui mHat (1,i) in
             if TupleSet.equal TupleSet.empty prnt then acc
             else Sparse.add (listPos mHat prnt) i acc) 
          m1 
          (Sparse.make (List.length mHat) (IntSet.cardinal m1)) in
  let (_,i_a0_b1) = 
       IntSet.fold 
          (fun i (c, acc) -> ((c+1), (Iso.add_exn i c acc)))
          vb1 
          (0, Iso.empty) in
  let (_,i_a1_b0) = 
       IntSet.fold 
          (fun i (c, acc) -> ((c+1), (Iso.add_exn i c acc)))
          vb0 
          (0, Iso.empty) in
  let i_d0_b0 = 
       IntSet.fold 
          (fun i acc -> 
             Iso.add_exn i (Iso.apply_exn i_d0_a1 i |> 
                            Iso.apply_exn i_a1_b0) acc) 
          (Iso.dom i_d0_a1 |> IntSet.of_list) 
          Iso.empty in
  let i_d1_b1 = 
       IntSet.fold 
          (fun i acc -> 
             Iso.add_exn i 
                (let inv = Iso.inverse i_a0_d1 in 
                Iso.apply_exn inv i |> Iso.apply_exn i_a0_b1) 
             acc) 
          (Iso.codom i_a0_d1 |> IntSet.of_list) 
          Iso.empty in
  let b0NS = 
       Sparse.fold 
          (fun n s acc -> 
             if List.mem n (Iso.dom i_d0_b0) 
                then Sparse.add (Iso.apply_exn i_d0_b0 n) s acc 
             else acc)
          (fst d).Big.p.Place.ns 
          (Sparse.make (IntSet.cardinal vb0) (IntSet.cardinal m0)) in
  let b1NS = 
       Sparse.fold 
          (fun n s acc -> 
             if List.mem n (Iso.dom i_d1_b1) 
                then Sparse.add (Iso.apply_exn i_d1_b1 n) s acc 
             else acc)
          (snd d).Big.p.Place.ns 
          (Sparse.make (IntSet.cardinal vb1) (IntSet.cardinal m1)) in
  let b0RN = 
       IntSet.fold 
          (fun i acc -> 
             match (rootPrntN i (snd a) red1) with
             | Some r -> Sparse.add (listPos mHat (rootEqui mHat (1,r))) 
                                    (Iso.apply_exn i_a1_b0 i) acc
             | None -> acc)
          vb0 
          (Sparse.make (List.length mHat) (IntSet.cardinal vb0)) in
  let b1RN = 
       IntSet.fold 
          (fun i acc -> 
             match (rootPrntN i (fst a) red0) with
             | Some r -> Sparse.add (listPos mHat (rootEqui mHat (0,r))) 
                                    (Iso.apply_exn i_a0_b1 i) acc
             | None -> acc)
          vb1 
          (Sparse.make (List.length mHat) (IntSet.cardinal vb1)) in
  let b0NN = 
       Sparse.fold 
          (fun nP nC acc -> 
             if List.mem nP (Iso.dom i_d0_b0) 
                then Sparse.add (Iso.apply_exn i_d0_b0 nP) 
                                (Iso.apply_exn i_d0_b0 nC) acc 
             else acc)
          (fst d).Big.p.Place.nn 
          (Sparse.make (IntSet.cardinal vb0) (IntSet.cardinal vb0)) in
  let b1NN = 
       Sparse.fold 
          (fun nP nC acc ->
             if List.mem nP (Iso.dom i_d1_b1) 
               then Sparse.add (Iso.apply_exn i_d1_b1 nP) 
                               (Iso.apply_exn i_d1_b1 nC) acc 
             else acc)
          (snd d).Big.p.Place.nn 
          (Sparse.make (IntSet.cardinal vb1) (IntSet.cardinal vb1)) in
  let (_, i_d0_b) = 
       IntSet.fold 
          (fun i (c, acc) -> ((c+1), (Iso.add_exn i c acc)))
          vb 
          (0, Iso.empty) in
  let i_d1_b = 
       IntSet.fold 
          (fun i acc ->
             let inv = Iso.inverse i_d0_d1 in
             Iso.add_exn i (Iso.apply_exn inv i 
                            |> Iso.apply_exn i_d0_b) acc)
          (Iso.codom i_d0_d1 |> IntSet.of_list) 
          Iso.empty in 
  let bRS = 
       List.fold_right 
          (fun t acc -> 
             let (i,r) = TupleSet.choose t in
             let b = (if i==0 then (fst d) else (snd d)) in
             match rootPrntS r b IntSet.empty with
             | Some par -> Sparse.add par (listPos mHat t) acc 
             | None -> acc)
          mHat 
          (Sparse.make (IntSet.cardinal p) (List.length mHat)) in
  let bNS = 
       List.fold_right 
          (fun t acc -> 
             let (i,r) = TupleSet.choose t in
             let b = (if i==0 then (fst d) else (snd d)) in
             let is = (if i==0 then i_d0_b else i_d1_b) in
             IntSet.fold 
                (fun n acc -> 
                   if List.mem n (Iso.dom is) 
                     then Sparse.add (Iso.apply_exn is n) 
                                     (listPos mHat t) acc
                   else acc)
                (Sparse.prn b.Big.p.Place.ns r) 
                acc)
          mHat 
          (Sparse.make (IntSet.cardinal vb) (List.length mHat)) in
  let bRN = 
       IntSet.fold 
          (fun n acc -> 
             match rootPrntN n (fst d) IntSet.empty with
             | Some par -> Sparse.add par (Iso.apply_exn i_d0_b n) acc
             | None -> acc)
          vb 
          (Sparse.make (IntSet.cardinal p) (IntSet.cardinal vb)) in
  let bNN = 
       IntSet.fold 
          (fun n acc -> 
             IntSet.fold 
                (fun par acc -> 
                   Sparse.add (Iso.apply_exn i_d0_b par) 
                              (Iso.apply_exn i_d0_b n) acc)
                (Sparse.prn (fst d).Big.p.Place.nn n) 
                acc)
          vb 
          (Sparse.make (IntSet.cardinal vb) (IntSet.cardinal vb)) in
  let placeB0 = {
      Place.r = List.length mHat; 
      n = IntSet.cardinal vb0; 
      s = IntSet.cardinal m0; 
      rn = b0RN;
      rs = b0RS; 
      nn = b0NN; 
      ns = b0NS} in 
  let placeB1 = {
      Place.r = List.length mHat; 
      n = IntSet.cardinal vb1; 
      s = IntSet.cardinal m1; 
      rn = b1RN;
      rs = b1RS; 
      nn = b1NN; 
      ns = b1NS} in 
  let placeB = {
      Place.r = IntSet.cardinal p; 
      n = IntSet.cardinal vb; 
      s = List.length mHat; 
      rn = bRN;
      rs = bRS; 
      nn = bNN; 
      ns = bNS} in
  let nodesB0 =
       Nodes.fold
          (fun nd ct acc ->
             if List.mem nd (Iso.dom i_d0_b0)
                then Nodes.add (Iso.apply_exn i_d0_b0 nd) ct acc
             else acc)
          (fst d).Big.n
          Nodes.empty in 
  let nodesB1 =
       Nodes.fold
          (fun nd ct acc ->
             if List.mem nd (Iso.dom i_d1_b1)
                then Nodes.add (Iso.apply_exn i_d1_b1 nd) ct acc
             else acc)
          (snd d).Big.n
          Nodes.empty in 
  let nodesB =
       Nodes.fold
          (fun nd ct acc ->
             if List.mem nd (Iso.dom i_d0_b)
                then Nodes.add (Iso.apply_exn i_d0_b nd) ct acc
             else acc)
          (fst d).Big.n
          Nodes.empty in 
  (* And return the calculated result *)
  (({Big.p = placeB0; l = Link.id_empty; n = nodesB0}, 
    {Big.p = placeB1; l = Link.id_empty; n = nodesB1}), 
    {Big.p = placeB; l = Link.id_empty; n = nodesB})
    
