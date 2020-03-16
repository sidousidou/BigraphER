(* A set of ports is a mapping from a node identifier to an integer *)

open Utils

include Map.Make (struct
    type t = int
    let compare = int_compare
  end)

let to_string ps = 
  "{" ^  (String.concat ", " (List.map (fun (a, b) ->
      "(" ^ (string_of_int a) ^ ", " ^ (colorise `bold (string_of_int b)) ^ ")"
    ) (bindings ps))) ^ "}"

let arities = bindings

(* assertion fails if a control is not in the signature *)
let of_nodes ns k =
  Node.fold (fun v c acc ->
      add v (safe (Sig.arity (Ctrl.name c) k)) acc
    ) ns empty

let to_IntSet ps =
  fold (fun v _ acc ->
      IntSet.add v acc
    ) ps IntSet.empty

let apply_exn ps iso =
  fold (fun v p acc ->
      add (Iso.find_exn v iso) p acc
    ) ps empty

(* total iso assumed *)
let apply ps iso = 
  fold (fun v p acc ->
      add (safe (Iso.find v iso)) p acc
    ) ps empty

let union a b =
  fold (fun v p acc ->
      try
        add v (p + (find v a)) acc
      with
      | Not_found -> add v p acc
    ) b a

(* node with min multiplicity *)
let min_p ps =
  try 
    fst (
      fold (fun v p (k, min) ->
          if p < min then  (v, p) else (k, min)
        ) ps (min_binding ps) 
    )
  with
  | Not_found -> assert false

(* Is a subset of b? *)
let rec subset a b n_a n_b =
  try ignore 
        (fold (fun v p acc ->
             let c = safe (Node.ctrl v n_a) in
             let compat_acc = filter (fun v' p' ->
                 p' >= p && Ctrl.(=) c (safe (Node.ctrl v' n_b))
               ) acc in
             if is_empty compat_acc then
               raise Exit
             else remove (min_p compat_acc) acc
           ) a b
        );
    true
  with
  | Exit -> false

let types ps ns =
  List.fast_sort String.compare 
    (fold (fun v p acc ->
         ((Ctrl.to_string (safe (Node.ctrl v ns))) ^ 
          "|" ^ 
          (string_of_int p)
         ) :: acc
       ) ps [])

let compat_list a b n_a n_b =
  fold (fun i p acc ->
      let c_i = safe (Node.ctrl i n_a) in
      let pairs = 
      fold (fun j p' acc ->
            if p =  p' && Ctrl.(=) c_i (safe (Node.ctrl j n_b)) then
              Cnf.M_lit (i, j) :: acc
            else acc
          ) b [] in
      pairs :: acc
    ) a []

let offset ps n = 
  fold (fun i p acc ->
      add (i + n) p acc
    ) ps empty
