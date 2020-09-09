type react = {
  name : string;
  rdx : Big.t;
  (* Redex --- lhs *)
  rct : Big.t;
  (* Reactum --- rhs *)
  eta : Fun.t option;
  (* Instantiation map *)
  w : float;
  (* Weight *)
  conds : AppCond.t list; (* Application conditions *)
}

type graph = {
  v : (int * Big.t) Base.H_int.t;
  e : (int * float * string) Base.H_int.t;
  l : int Base.H_predicate.t;
  preds : Base.S_predicate.t;
}

module type T = sig
  include
    Rs.RS
      with type react = react
       and type ac := AppCond.t
       and type label = float
       and type graph = graph
       and type limit = int

  val weight : react -> label
end

module Make (S : Solver.M) = struct
  module AC = AppCond.Make (S)

  module R =
    React.Make (S) (AC)
      (struct
        type ac = AppCond.t

        type t = react

        type label = float

        let name r = r.name

        let lhs r = r.rdx

        let rhs r = r.rct

        let conds r = r.conds

        let l r = r.w

        let map r = r.eta

        let merge_occ (b, p, r) (_, p', r') = (b, p +. p', r @ r')

        let equal r r' =
          S.equal r.rdx r'.rdx && S.equal r.rct r'.rct
          && Base.opt_equal Fun.equal r.eta r'.eta
          && r.w = r'.w

        let val_chk r = r.w >= 0.0

        let val_chk_error_msg = "Not a valid weight"

        let string_of_label = Printf.sprintf "%-3g"

        let make ~name ~lhs ~rhs ?conds:(c = []) w eta =
          { name; rdx = lhs; rct = rhs; eta; w; conds = c }

        (* Normalise a list of occurrences *)
        let step_post (l, n) =
          let sum = List.fold_left (fun acc (_, w, _) -> acc +. w) 0.0 l in
          (List.map (fun (b, w, r) -> (b, w /. sum, r)) l, n)

        (* Pick the first reaction rule with probability > limit, normalising
           its probability by subtracting the probability of the previous
           rule. *)
        let pick limit = function
          | [] -> None
          | ((_b, p, _rr) as head) :: tail ->
              let rec _pick limit (b', p', rr') = function
                | ((b, p, rr) as element) :: tail ->
                    if p > limit then Some (b, p -. p', rr)
                    else _pick limit element tail
                | [] -> Some (b', p', rr')
              in
              if p > limit then Some head else _pick limit head tail

        let random_step_post (ss, m) =
          match ss with
          | [] -> (None, m)
          | _ ->
              (* Sort transitions by probability *)
              let ss_sort =
                List.fast_sort (fun (_, a, _) (_, b, _) ->
                    compare (a : float) (b : float))
              (* Compute cumulative probability *)
              and cumulative =
                List.fold_left
                  (fun (out, cum_p) (b, p, r) ->
                    let cum_p' = cum_p +. p in
                    ((b, cum_p', r) :: out, cum_p'))
                  ([], 0.0)
              in
              let reaction_rules, cum_p = ss_sort ss |> cumulative in
              List.rev reaction_rules |> pick (Random.float cum_p)
              |> fun x -> (x, m)
      end)

  module P =
    Priority.Make (S) (R)
      (struct
        let f_val _ = true

        (* TODO: Rules should not be applied unless the /normalised/ probability is 1,
         * which means we need the match first *)
        let f_r_val _ = true
      end)

  module G = struct
    type t = graph

    type l = R.label

    let init n preds =
      Base.
        {
          v = H_int.create n;
          e = H_int.create n;
          l = H_predicate.create n;
          preds = S_predicate.of_list preds;
        }

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

  module K = struct
    let typ = Rs.PBRS
  end

  include Rs.Make (S) (R) (P) ((L : Rs.L with type l = R.label))
            ((
            G : Rs.G with type l = R.label ))
            (K)

  let weight = label
end
