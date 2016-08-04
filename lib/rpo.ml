
let makeSet iso =
  IntSet.of_list (Iso.codom iso)
 
(* RPO algorithm *)
(* The isomorphism shows for each bigraph which node (domain) is connected to which concrete node (codomain)
   in the rpo model *)
let rpo a0 iso_a0 a1 iso_a1 d0 iso_d0 d1 iso_d1 =
  let va0 = makeSet iso_a0 in
  let va1 = makeSet iso_a1 in
  let vd0 = makeSet iso_d0 in
  let vd1 = makeSet iso_d1 in
  (* Now to the actual algorithm *)
  let vb0 = IntSet.diff va1 va0 in
  let vb1 = IntSet.diff va0 va1 in
  let vb = IntSet.inter vd0 vd1 in
  (* Only dummy code for compilation *)
  3 + 4
  
