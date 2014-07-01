(* Module for nodes. When it contains n elements the indexes are from 0 to n-1 *)

include Map.Make (struct
    type t = int
    let compare a b = a - b 
  end)

let to_string s =
  "{" ^ (String.concat "," (List.map (fun (v, c) ->
      "(" ^ (string_of_int v) ^ ", " ^ (Ctrl.to_string c) ^ ")"
    ) (bindings s))) ^ "}" 

let to_dot s =
  String.concat "\n" (List.map (fun (v, c) ->
      let n = Ctrl.to_string c in
      Printf.sprintf "v%d [ label=\"%s\", shape=ellipse,\
                      fontname=\"sans-serif\", fontsize=9.0,\
                      fixedsize=true, width=%f, height=.30 ];" 
	v n (0.1 *. (float (String.length n)) +. 0.2)
    ) (bindings s))

(* n is the size of a *)
let tens a b n =
  fold (fun v c acc ->
      add (v + n) c acc
    ) b a
            
let apply_exn s iso =
  fold (fun v c acc ->
      add (Iso.find_exn v iso) c acc
    ) s empty
    
(* Only nodes in the domain of the isomorphism are transformed. Other nodes are discarded. *)    
let filter_apply_iso s iso =
  fold (fun v c acc ->
      match Iso.find v iso with
      | Some v' -> add v' c acc
      | None -> acc
    ) s empty
  
let parse s =
  let tokens = Str.split (Str.regexp_string " ") s in
  fst (List.fold_left (fun (acc, i) t ->
      (add i (Ctrl.parse t) acc, i + 1)
    ) (empty, 0) tokens
    )

(* There exists a control in a that is not in b *)
let not_sub a b =
  exists (fun _ c ->
      for_all (fun _ c' ->
          Ctrl.compare c c' != 0
        ) b 
    ) a

(* Ordered list of controls *)
let norm s =
  List.fast_sort Ctrl.compare 
    (List.map Ctrl.norm (snd (List.split (bindings s))))

let equal a b =
  try
    List.for_all2 (fun c0 c1 -> 
        Ctrl.compare c0 c1 = 0
      ) (norm a) (norm b)
  with
  | Invalid_argument _ -> false

let find_exn = find

let ctrl v s =
  try Some (find_exn v s)
  with
  | Not_found -> None

(* Find nodes of control c *)
let same_ctrl c s =
  fold (fun i c' acc ->
      if Ctrl.(=) c c' then
        IntSet.add i acc
      else acc
    ) s IntSet.empty

let apply_subs s subs =
  fold (fun i c acc ->
      add i (Ctrl.apply c subs) acc      
    ) s empty

let is_fun =
  exists (fun _ c -> Ctrl.is_fun c)
