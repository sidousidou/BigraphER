type react =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    rate : float                   (* Reaction rate *)
  }

module RT = struct
  type t = react
  type label = float 
  type occ = Big.bg * float
  type edge = int * float

  let lhs r = r.rdx

  let rhs r = r.rct

  let l r = Some r.rate

  let map r = r.eta

  let val_chk r = r.rate > 0.0

  let val_chk_error_msg = "Not a stochastic rate"
    
  let string_of_label = function
    | Some r -> Printf.sprintf "%-3g" r
    | None -> assert false (*BISECT-IGNORE*)
      
  let parse ~lhs ~rhs r eta =
    match r with
    | None -> assert false (*BISECT-IGNORE*)
    | Some r -> { rdx = lhs;
                  rct = rhs;
                  eta = eta;
                  rate = r; }
                
  let to_occ b r = (b, r.rate)

  let big_of_occ (b, _) = b

  let merge_occ (b, rho) (_, rho') = (b, rho +. rho')

  let update_occ (_, rho) b' = (b', rho)

  let edge_of_occ (_, rho) i = (i, rho)

  let step b rules = RrType.gen_step b rules
      ~big_of_occ ~to_occ ~merge_occ ~lhs ~rhs ~map

  let random_step b rules =
    (* Sort transitions by rate *)
    let (ss, m) = step b rules in
    let ss_sorted =
      List.fast_sort (fun a b ->
          compare (snd a) (snd b))
        ss in
    (* Compute exit rate *)
    let a0 =
      List.fold_left (fun acc (_, rho) -> acc +. rho) 0.0 ss in
    let r = (Random.float 1.0) *. a0 in
    let rec aux acc = function
      | (s, rho) :: ss ->
        let acc' = acc +. rho in
        if acc' > r then
          (let tau =
             (1. /. a0) *. (log (1. /. (Random.float 1.0))) in
           (Some (s, tau), m))
        else aux acc' ss
      | [] -> (None, m) in
    aux 0.0 ss_sorted
  
end

module R = RrType.Make (RT)

let is_inst r =
  match  R.l r with
  | Some r -> r = infinity
  | None -> assert false (*BISECT-IGNORE*)

module PT = struct
  type t = R.t list
  let f_val rr = not (List.exists is_inst rr)
  let f_r_val = List.for_all is_inst				   
end

module H_int = Base.H_int

module H_string = Base.H_string
                
module S_string = Base.S_string

type graph = { v : (int * Big.bg) H_int.t;
               e : R.edge H_int.t;
               l : int H_string.t;  
               preds : S_string.t; }

module G = struct
  type t = graph
  type edge_type = RT.edge	       
  let init n preds =
    { v = H_int.create n;
      e = H_int.create n;
      l = H_string.create n;
      preds = S_string.of_list preds; }		      
  let states g = g.v
  let label g = (g.preds, g.l)
  let edges g = g.e
  let dest u = fst u
  let string_of_arrow u = Printf.sprintf "%.4g" (snd u)
end

module L = struct
  type t = float
  type occ = R.occ
  let init = 0.0
  let increment t o = t +. (snd (R.edge_of_occ o 0))
  let is_greater = ( > )
  let to_string = Printf.sprintf "%.4g" 
end

module T = struct
  let typ = Rs.SBRS
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (T)
