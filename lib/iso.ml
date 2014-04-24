include Map.Make (struct
    type t = int
    let compare a b = a - b
  end)
    
let inverse iso =
  fold (fun i j iso' ->
      add j i iso') iso empty

let dom iso =
  fst (List.split (bindings iso))


let codom iso = 
  snd (List.split (bindings iso))

let to_list = bindings

let 


  let map iso i_dom i_codom = 
    let iso' = Hashtbl.create (Hashtbl.length iso) in
    Hashtbl.iter (fun i j ->
        Hashtbl.add iso' (find i_dom i) (find i_codom j)
      ) iso;
    iso'
  

  let union a b =
    let u = Hashtbl.create ((Hashtbl.length a) + (Hashtbl.length b)) in 
    let f i j = add u i j in
    Hashtbl.iter f a;
    Hashtbl.iter f b;
    u

 

 
  
  exception COMPARE of int  

  let subseteq a b =     
    try
      iter (fun i j ->
	match j - (find b i) with
	| 0 -> ()
	| x -> raise (COMPARE x)) a;
      0
    with
    | Not_found -> 1
    | COMPARE x -> x

  let compare a b = 
    let x = subseteq a b
    and y = subseteq b a in
    match x with
    | 0 -> y
    | _ -> x
      
  let equal a b = 
    compare a b = 0  

  let to_string iso =
    sprintf "{%s}" 
      (String.concat ", " 
	 (Hashtbl.fold (fun i j acc -> 
	   (sprintf "(%d, %d)" i j) :: acc) iso []))

  let of_list l =
    let iso = Hashtbl.create (List.length l) in
    List.iter (fun (i, j) ->
      Hashtbl.add iso i j) l;
    iso



  let is_id iso =
    Hashtbl.fold (fun i j acc -> 
      (i = j) && acc) iso true

  (* input:  i : P -> T  autos : P -> P *)
  let gen_isos i autos =
    let apply iso a = 
      assert (cardinal iso = cardinal a);
      fold (fun i j acc ->
	  add acc (find a i) j; 
          acc
        ) iso (empty ()) in
    List.map (apply i) autos
