(* Power set over integers *)
module PS =
  struct include Base.S_opt
                   (Set.Make (IntSet))
                   (struct
                     type t = IntSet.t

                     let pp = IntSet.pp
                   end)

         let pp =
           pp ~open_b:Format.pp_open_hbox
             ~first:(fun out -> Format.pp_print_string out "{")
             ~last:(fun out -> Format.pp_print_string out "}")
             ~sep:(fun out -> Format.pp_print_string out ",")

  end

(* Compute the cyclic decomposition of a permutation sigma *)
let orbits sigma =
  let rec cycle x res =
    let x' = Base.safe @@ Iso.apply sigma x in
    if IntSet.mem x' res then res
    else cycle x' (IntSet.add x' res) in
  let rec loop (res : PS.t) dom =
    match IntSet.min_elt dom with
    | None -> res
    | Some x -> let orb = cycle x (IntSet.singleton x) in
                loop (PS.add orb res) (IntSet.diff dom orb) in
  IntSet.iso_dom sigma |> loop PS.empty
  |> PS.filter (fun x -> not (IntSet.is_singleton x))
