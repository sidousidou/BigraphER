type react = {
  name : string;
  action : string;
  reward : int;
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
  e : (int * (string * int * float) * string) Base.H_int.t;
  l : int Base.H_predicate.t;
  preds : Base.S_predicate.t;
}

module type T = sig
  include
    Rs.RS
      with type react = react
       and type ac := AppCond.t
       and type label = string * int * float
       and type graph = graph
       and type limit = int

  val action : react -> string

  val weight : react -> float
end

module Make (S : Solver.M) = struct
  module AC = AppCond.Make (S)

  module R = struct
    include React.Make (S) (AC)
              (struct
                type ac = AppCond.t

                type t = react

                type label = string * int * float

                let name r = r.name

                let action r = r.action

                let lhs r = r.rdx

                let rhs r = r.rct

                let conds r = r.conds

                let l r = (r.action, r.reward, r.w)

                let map r = r.eta

                let merge_occ (b, (a, rew, p), r) (_, (_, _, p'), r') =
                  (b, (a, rew, p +. p'), r @ r')

                let equal r r' =
                  r.action = r'.action && S.equal r.rdx r'.rdx
                  && S.equal r.rct r'.rct
                  && Base.opt_equal Fun.equal r.eta r'.eta
                  && r.w = r'.w

                let val_chk r = r.w >= 0.0

                let val_chk_error_msg = "Not a valid weight"

                let string_of_label (action, reward, probability) =
                  Printf.sprintf "%s %d %-3g" action reward probability

                let make ~name ~lhs ~rhs ?conds:(c = []) (action, reward, w)
                    eta =
                  {
                    name;
                    action;
                    reward;
                    rdx = lhs;
                    rct = rhs;
                    eta;
                    w;
                    conds = c;
                  }

                (* Normalise a list of occurrences *)
                let step_post (l, n) =
                  let normalise (l, n) =
                    let sum =
                      List.fold_left
                        (fun acc (_, (_, _, p), _) -> acc +. p)
                        0.0 l
                    in
                    ( List.map
                        (fun (b, (a, rew, p), r) ->
                          (b, (a, rew, p /. sum), r))
                        l,
                      n )
                  in
                  let rec remove_duplicates = function
                    | [] -> []
                    | h :: t ->
                        let filtered = List.filter (fun x -> x <> h) t in
                        h :: remove_duplicates filtered
                  in
                  let different_actions =
                    List.map (fun (_, _, r) -> action (List.hd r)) l
                    |> remove_duplicates
                  in
                  ( List.fold_left
                      (fun acc act ->
                        let reaction_rules =
                          List.filter
                            (fun (_, _, r) -> action (List.hd r) = act)
                            l
                        in
                        let normalised, _ = normalise (reaction_rules, 0) in
                        normalised @ acc)
                      [] different_actions,
                    n )

                (* Pick the first reaction rule with probability > limit,
                   normalising its probability by subtracting the probability
                   of the previous rule. *)
                let pick limit = function
                  | [] -> None
                  | ((_b, (_a, _r, p), _rr) as head) :: tail ->
                      let rec _pick limit (b', (a', r', p'), rr') = function
                        | ((b, (a, r, p), rr) as element) :: tail ->
                            if p > limit then Some (b, (a, r, p -. p'), rr)
                            else _pick limit element tail
                        | [] -> Some (b', (a', r', p'), rr')
                      in
                      if p > limit then Some head else _pick limit head tail

                let random_step_post (ss, m) =
                  match ss with
                  | [] -> (None, m)
                  | _ ->
                      (* Sort transitions by normalised probability *)
                      let ss_sort =
                        List.fast_sort (fun (_, a, _) (_, b, _) ->
                            compare a b)
                      (* Compute cumulative probability *)
                      and cumulative =
                        List.fold_left
                          (fun (out, cum_p) (b, (a, rew, p), r) ->
                            let cum_p' = cum_p +. p in
                            ((b, (a, rew, cum_p'), r) :: out, cum_p'))
                          ([], 0.0)
                      in
                      let reaction_rules, cum_p = ss_sort ss |> cumulative in
                      List.rev reaction_rules |> pick (Random.float cum_p)
                      |> fun x -> (x, m)
              end)
  end

  (* Priorities *)
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

  module K = struct
    let typ = Rs.NBRS
  end

  include Rs.Make (S) (R) (P) ((L : Rs.L with type l = R.label))
            ((
            G : Rs.G with type l = R.label ))
            (K)

  let weight r = match label r with _, _, w -> w

  let action r = match label r with a, _, _ -> a
end
