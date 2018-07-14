type react =
  { name : string;
    rdx  : Big.t;                  (* Redex   --- lhs   *)
    rct  : Big.t;                  (* Reactum --- rhs   *)
    eta  : Fun.t option;           (* Instantiation map *)
    p    : float                   (* Probability       *)
  }

module RT = struct

  type t = react

  type label = float

  let name r = r.name

  let action _ = ""

  let lhs r = r.rdx

  let rhs r = r.rct

  let l r = r.p

  let equal r r' =
    Big.equal r.rdx r'.rdx
    && Big.equal r.rct r'.rct
    && Base.opt_equal Fun.equal r.eta r'.eta
    && r.p = r'.p

  let map r = r.eta

  let merge_occ (b, p, r) (_, p', r') = (b, p +. p', r @ r')

  let val_chk r = r.p > 0.0 && r.p <= 1.0

  let val_chk_error_msg = "Not a probability"

  let string_of_label = Printf.sprintf "%-3g"

  let parse ~name ?(action="") ~lhs ~rhs p eta =
    { name = name;
      rdx  = lhs;
      rct  = rhs;
      eta  = eta;
      p    = p; }

  (* Normalise a list of occurrences *)
  let norm (l, n) =
    let sum = List.fold_left (fun acc (_, p, _) -> acc +. p) 0.0 l in
    (List.map (fun (b, p, r) -> (b, p /. sum, r)) l, n)

  let step b rules =
    RrType.gen_step b rules merge_occ ~lhs ~rhs ~label:l ~map
    |> norm

  let random_step b rules =
    let (ss, m) = step b rules in
    match ss with
    | [] -> (None, m)
    | _ ->
      begin
        (* Sort transitions by probability *)
        let ss_sort =
          List.fast_sort (fun (_, a, _) (_, b, _) -> compare a b)
        (* Compute cumulative probability *)
        and cumulative =
          List.fold_left (fun (out, cum_p) (b, p, r) ->
              let cum_p' = cum_p +. p in
              ((b, cum_p', r) :: out, cum_p'))
            ([], 0.0)
        and pick =
          List.find (fun (_, p, _) -> p > (Random.float 1.0)) in
        ss_sort ss
        |> cumulative
        |> fst
        |> List.rev
        |> pick
        |> (fun x -> (Some x, m))

      end
end

module R = RrType.Make (RT)

let is_determ r =
  R.l r = 1.0

module PT = struct
  type t = R.t list
  let f_val _ = true
  let f_r_val = List.for_all is_determ
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
  type t = int
  type l = R.label
  let init = 0
  let increment t _ = t + 1
  let is_greater = ( > )
  let to_string = string_of_int
end

module T = struct
  let typ = Rs.PBRS
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (T)

let prob r = r.p

