type react =
  { name   : string;
    action : string;
    reward : int;
    rdx    : Big.t;                  (* Redex   --- lhs   *)
    rct    : Big.t;                  (* Reactum --- rhs   *)
    eta    : Fun.t option;           (* Instantiation map *)
    p      : float                   (* Probability       *)
  }

module RT = struct

  type t = react

  type label = string * int * float

  let name r = r.name

  let action r = r.action

  let lhs r = r.rdx

  let rhs r = r.rct

  let l r = r.action, r.reward, r.p

  let equal r r' =
    r.action = r'.action
    && Big.equal r.rdx r'.rdx
    && Big.equal r.rct r'.rct
    && Base.opt_equal Fun.equal r.eta r'.eta
    && r.p = r'.p

  let map r = r.eta

  let merge_occ (b, (a, rew, p), r) (_, (_, _, p'), r') =
    (b, (a, rew, p +. p'), r @ r')

  let val_chk r = r.p > 0.0 && r.p <= 1.0

  let val_chk_error_msg = "Not a probability"

  let string_of_label (action, reward, probability) =
    Printf.sprintf "%s %d %-3g" action reward probability

  let parse ~name ~lhs ~rhs (action, reward, p) eta =
    { name   = name;
      action = action;
      reward = reward;
      rdx    = lhs;
      rct    = rhs;
      eta    = eta;
      p      = p; }

  (* Normalise a list of occurrences *)
  let norm (l, n) =
    let normalise (l, n) =
      let sum = List.fold_left (fun acc (_, (_, _, p), _) -> acc +. p) 0.0 l in
      (List.map (fun (b, (a, rew, p), r) -> (b, (a, rew, p /. sum), r)) l, n)
    in
    let rec remove_duplicates = function
      | [] -> []
      | h :: t ->
        let filtered = List.filter (fun x -> x <> h) t in
        h :: remove_duplicates filtered
    in
    let different_actions = List.map (fun (_, _, r) -> action (List.hd r)) l
                            |> remove_duplicates in
    List.fold_left (fun acc act ->
        let reaction_rules = List.filter
            (fun (_, _, r) -> action (List.hd r) = act) l in
        let (normalised, _) = normalise (reaction_rules, 0) in
        normalised @ acc
      ) [] different_actions, n

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
          List.fold_left (fun (out, cum_p) (b, (a, rew, p), r) ->
              let cum_p' = cum_p +. p in
              ((b, (a, rew, cum_p'), r) :: out, cum_p'))
            ([], 0.0)
        and pick =
          List.find (fun (_, (_, _, p), _) -> p > (Random.float 1.0)) in
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
  let third (_, _, x) = x in
  third (R.l r) = 1.0

module PT = struct
  type t = R.t list
  let f_val _ = true
  let f_r_val = List.for_all is_determ
end

module H_int = Base.H_int

module H_predicate = Base.H_predicate

module S_predicate = Base.S_predicate

type graph = { v : (int * Big.t) H_int.t;
               e : (int * R.label * string) H_int.t;
               l : int H_predicate.t;
               preds : S_predicate.t; }

module G = struct
  type t = graph
  type l = R.label
  let init n preds =
    { v = H_int.create n;
      e = H_int.create n;
      l = H_predicate.create n;
      preds = S_predicate.of_list preds; }
  let states g = g.v
  let label g = (g.preds, g.l)
  let edges g = g.e
  let string_of_l (action, reward, probability) =
    Printf.sprintf "%s %d %.4g" action reward probability
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
  let typ = Rs.NBRS
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (T)

let prob r = r.p

let action r = r.action