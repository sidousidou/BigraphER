type react =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    p : float                      (* Probability *)
  }

module RT = struct
  type t = react
  type label = float 
  type occ = Big.bg * float
  type edge = int * float

  let lhs r = r.rdx
  let rhs r = r.rct
  let l r = r.p
  let map r = r.eta
  let string_of_label = Printf.sprintf "%-3g"
  let val_chk r = r.p > 0.0 && r.p <= 1.0
  let val_chk_error_msg = "Not a probability"
  let to_occ b r = (b, r.p)
  let big_of_occ (b, _) = b
  let merge_occ (b, p) (_, p') = (b, p +. p')
  let update_occ (_, p) b' = (b', p)
  let edge_of_occ (_, p) i = (i, p)

  (* Normalise a list of occurrences *)
  let norm (l, n) =
    let sum = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 l in
    (List.map (fun (b, p) -> (b, p /. sum)) l, n)
  
  let step b rules =
    RrType.gen_step b rules
      ~big_of_occ ~to_occ ~merge_occ ~lhs ~rhs ~map
    |> norm
      
  let random_step b rules =
    let (ss, m) = step b rules in
    match ss with
    | [] -> (None, m)
    | _ ->
      begin
        (* Sort transitions by probability *)
        let ss_sort =
          List.fast_sort (fun a b -> compare (snd a) (snd b))
        (* Compute cumulative probability *)
        and cumulative =
          List.fold_left (fun (out, cum_p) (b, p) ->
              let cum_p' = cum_p +. p in
              ((b, cum_p') :: out, cum_p'))
            ([], 0.0)
        and pick =
          List.find (fun (_, p) -> p > (Random.float 1.0)) in
        ss_sort ss
        |> cumulative
        |> fst
        |> List.rev
        |> pick
        |> (fun x -> (Some x, m))
           
      end
  
end

module R = RrType.Make (RT)

let is_determ r = R.l r = 1.0

module PT = struct
  type t = R.t list
  let f_val _ = true
  let f_r_val = List.for_all is_determ				   
end

module H_int = Base.H_int

module H_string = Base.H_string

type graph = { v : (int * Big.bg) H_int.t;
               e : R.edge H_int.t;
               l : int H_string.t; }

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
  let string_of_arrow u = Printf.sprintf "%.4g" (snd u)
end

module L = struct
  type t = int
  type occ = R.occ
  let init = 0
  let increment t _ = t + 1
  let is_greater = ( > )
  let to_string = string_of_int
end

module T = struct
  let typ = Rs.SBRS
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (T)
