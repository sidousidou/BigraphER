type graph =
  { adjmat : int array array;
    colours : int array;
    n      : int;
  }

type bidomain =
  {
    left: int list;
    right: int list;
  }

(* Creates [start; start+1; ... ; one_past_end-1] *)
let range start one_past_end =
  let rec aux a b lst =
    if a==b then lst else a::(aux (a+1) b lst) in
  aux start one_past_end []

let min_set_size bidomain =
  min (List.length bidomain.left) (List.length bidomain.right)

let max_set_size bidomain =
  max (List.length bidomain.left) (List.length bidomain.right)

let bound current bidomains =
  List.fold_left (+) (List.length current) (List.map min_set_size bidomains)

let sort_bidomains bidomains =
  List.sort (fun b0 b1 -> max_set_size b0 - max_set_size b1) bidomains

let filter_bidomain g0_adjrow g1_adjrow bidomain edge_type =
  { left = List.filter (fun u -> g0_adjrow.(u)==edge_type) bidomain.left;
    right = List.filter (fun u -> g1_adjrow.(u)==edge_type) bidomain.right
  }

let filter g0 g1 v w bidomains =
  let head = List.hd bidomains in
  let tail = List.tl bidomains in
  let bidomains' = { left = List.filter (fun u -> u!=v) head.left;
                     right = List.filter (fun u -> u!=w) head.right;
                   } :: tail in
  let fn = fun bidomain ->
    List.map (filter_bidomain g0.adjmat.(v) g1.adjmat.(w) bidomain) [0;1;2;3] in
  List.map fn bidomains'
    |> List.concat
    |> List.filter (fun b -> min_set_size b > 0)

let remove_v v bidomains =
  let head = List.hd bidomains in
  let tail = List.tl bidomains in
  if List.length head.left == 1 then tail
  else
    let head' =
      { left = List.filter (fun u -> u!=v) head.left;
        right = head.right;
      } in
    head' :: tail

let rec search validate_mapping g0 g1 incumbent current bidomains =
  let incumbent' =
    if validate_mapping current then
      (
        if      List.length current <  List.length (List.hd incumbent) then incumbent
        else if List.length current == List.length (List.hd incumbent) then current :: incumbent
        else [current]
      )
    else [current] in
  let best_mapping_sz = List.length (List.hd incumbent') in
  if List.length bidomains==0 || bound current bidomains < best_mapping_sz then incumbent'
  else
    let bidomains' = sort_bidomains bidomains in
    let head = List.hd bidomains' in
    let v = List.hd head.left in
    (* try mapping v to each w in turn *)
    let incumbent'' =
      let fn = fun incumb w ->
        search validate_mapping g0 g1 incumb ((v,w)::current) (filter g0 g1 v w bidomains') in
      List.fold_left fn incumbent' head.right in 
    (* try leaving vertex v unassigned *)
    search validate_mapping g0 g1 incumbent'' current (remove_v v bidomains')

let adjrow_deg adjrow =
  (Array.fold_left (fun a b -> if (b land 1) == 1 then a+1 else a) 0 adjrow +
   Array.fold_left (fun a b -> if (b land 2) == 2 then a+1 else a) 0 adjrow)

let vv_order g =
  range 0 g.n
    |> List.sort (fun v w -> adjrow_deg g.adjmat.(v) - adjrow_deg g.adjmat.(w))
    |> List.rev 
    |> Array.of_list

let induced_subgraph g vv =
  let n = Array.length vv in
  let colours = vv |> Array.map (fun v -> g.colours.(v)) in
  let adjmat = vv
    |> Array.map (fun v -> Array.map (fun w -> g.adjmat.(v).(w)) vv) in
  { adjmat = adjmat;
    colours = colours;
    n      = n
  }

let all_colours g0 g1 =
  let cc0 = Array.to_list g0.colours in
  let cc1 = Array.to_list g1.colours in
  let cc = List.sort (-) (List.append cc0 cc1) in
  Printf.printf "cc len %i\n" (List.length cc);
  let rec dedup lst =
    match lst with
    | [] -> []
    | hd :: [] -> [hd] 
    | f :: s :: tl -> if f==s then dedup (s::tl) else f::(dedup (s::tl)) in
  dedup cc

let get_label_class g0 g1 colour =
  { left = List.filter (fun v -> g0.colours.(v)==colour) (range 0 g0.n);
    right = List.filter (fun v -> g1.colours.(v)==colour) (range 0 g1.n)
  }

let get_label_classes g0 g1 =
  let colours = all_colours g0 g1 in
  Printf.printf "Number of colours %i\n" (List.length colours);
  List.map (get_label_class g0 g1) colours |> List.filter (fun b -> min_set_size b > 0)

let renumber_solution vv0 vv1 sol =
    List.map (fun m -> List.map (fun pair -> vv0.(fst pair), vv1.(snd pair)) m) sol

let mcs g0 g1 validate_mapping =
  let vv0 = vv_order g0 in
  let vv1 = vv_order g1 in
  let g0' = induced_subgraph g0 vv0 in
  let g1' = induced_subgraph g1 vv1 in 
  let initial_label_classes = get_label_classes g0' g1' in
  search validate_mapping g0' g1' [[]] [] initial_label_classes
    |> renumber_solution vv0 vv1
    |> List.map (fun m -> List.sort (fun pair0 pair1 -> fst pair0 - fst pair1) m)
