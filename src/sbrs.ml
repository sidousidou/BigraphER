type react =
  { name : string;
    rdx  : Big.t;                  (* Redex   --- lhs   *)
    rct  : Big.t;                  (* Reactum --- rhs   *)
    eta  : Fun.t option;           (* Instantiation map *)
    rate : float                   (* Reaction rate     *)
  }

module RT = struct

  type t = react

  type label = float

  let name r = r.name

  let action _ = ""

  let lhs r = r.rdx

  let rhs r = r.rct

  let l r = r.rate

  let equal r r' =
    Big.equal r.rdx r'.rdx
    && Big.equal r.rct r'.rct
    && Base.opt_equal Fun.equal r.eta r'.eta
    && r.rate = r'.rate

  let map r = r.eta

  let merge_occ (b, rho, r) (_, rho', r') = (b, rho +. rho', r @ r')

  let val_chk r = r.rate > 0.0

  let val_chk_error_msg = "Not a stochastic rate"

  let string_of_label = Printf.sprintf "%-3g"

  let parse ~name ~lhs ~rhs r eta =
    { name = name;
      rdx  = lhs;
      rct  = rhs;
      eta  = eta;
      rate = r; }

  let step b rules =
    RrType.gen_step b rules merge_occ ~lhs ~rhs ~label:l ~map

  let random_step b rules =
    (* Sort transitions by rate *)
    let (ss, m) = step b rules in
    let ss_sorted =
      List.fast_sort (fun (_, a, _) (_, b, _) ->
          compare a b)
        ss in
    (* Compute exit rate *)
    let a0 =
      List.fold_left (fun acc (_, rho, _) -> acc +. rho) 0.0 ss in
    let r = (Random.float 1.0) *. a0 in
    let rec aux acc = function
      | (s, rho, reaction_rules) :: ss ->
        let acc' = acc +. rho in
        if acc' > r then begin
          let tau =
            (1. /. a0) *. (log (1. /. (Random.float 1.0))) in
          (Some (s, tau, reaction_rules), m) end
        else aux acc' ss
      | [] -> (None, m) in
    aux 0.0 ss_sorted

end

module R = RrType.Make (RT)

let is_inst r =
  R.l r = infinity

module PT = struct
  type t = R.t list
  let f_val rr = not (List.exists is_inst rr)
  let f_r_val = List.for_all is_inst
end

module H_int = Base.H_int

module H_string = Base.H_string

module S_string = Base.S_string

type graph = { v : (int * Big.t) H_int.t;
               e : (int * R.label * string) H_int.t;
               l : int H_string.t;
               preds : S_string.t; }

module G = struct
  type t = graph
  type l = R.label
  let init n preds =
    { v = H_int.create n;
      e = H_int.create n;
      l = H_string.create n;
      preds = S_string.of_list preds; }
  let states g = g.v
  let label g = (g.preds, g.l)
  let edges g = g.e
  let string_of_l = Printf.sprintf "%.4g"
end

module L = struct
  type t = float
  type l = R.label
  let init = 0.0
  let increment t l = t +. l
  let is_greater = ( > )
  let to_string = Printf.sprintf "%.4g"
end

module T = struct
  let typ = Rs.SBRS
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (T)

let rate r = r.rate
