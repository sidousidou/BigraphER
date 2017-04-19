type sreact =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    rate : float
  }

module RT = struct
  type t = sreact
  type label = float 
  type occ = Big.bg * float
  type edge = int * float

  let lhs r = r.rdx
  let rhs r = r.rct
  let l r = r.rate
  let map r = r.eta
  let string_of_label l = string_of_float l
  let val_chk r = r.rate > 0.0
  let val_chk_error_msg = "Non positive rate"
  let to_occ b r = (b, r.rate)
  let big_of_occ (b, _) = b
  let merge_occ (b, rho) (_, rho') = (b, rho +. rho')
  let update_occ (_, rho) b' = (b', rho)
  let edge_of_occ (_, rho) i = (i, rho)
 let step b rules = RrType.gen_step b rules
      ~big_of_occ ~to_occ ~merge_occ ~lhs ~rhs ~map
  let random_step step_f b rules =
    (* Sort transitions by rate *)
    let (ss, m) = step_f b rules in
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

let is_inst r = R.l r = infinity

module PT = struct
  type t = R.t list
  let f_val rr = not (List.exists is_inst rr)
  let f_r_val = List.for_all is_inst				   
end

module H_int = Base.H_int

module H_string = Base.H_string

type graph = { v : (int * Big.bg) H_int.t;
               e : R.edge H_int.t;
               l : int H_string.t;  
             }

type stats =  { time : float; 
                states : int;  
                trans : int;  
                occs : int;
              }

module G = struct
  type t = graph
  type edge_type = RT.edge	       
  let init n =
    { v = H_int.create n;
      e = H_int.create n;
      l = H_string.create n; }		      
  let states g = g.v
  let label g = g.l
  let edges g = g.e
  let dest u = fst u
  let string_of_arrow u = string_of_float (snd u)
end

module S = struct
  type t = stats
  type g = graph
  let make t0 g m =
    { time = (Unix.gettimeofday () -. t0);
      states = H_int.length g.v; 
      trans = H_int.length g.e;
      occs = m; }
end	       

module L = struct
  type t = float
  type occ = R.occ
  let init = 0.0
  let increment t o = t +. (snd (R.edge_of_occ o 0))
  let is_greater = ( > )
end

module T = struct
  let typ = "SBRS"
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (S) (T)
