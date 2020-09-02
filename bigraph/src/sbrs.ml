type react = {
  name : string;
  rdx : Big.t;
  (* Redex --- lhs *)
  rct : Big.t;
  (* Reactum --- rhs *)
  eta : Fun.t option;
  (* Instantiation map *)
  rate : float;
  (* Reaction rate *)
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
       and type limit = float

  val rate : react -> float
end

module Make (S : Solver.M) = struct
  module AC = AppCond.Make (S)

  module R = struct
    include React.Make (S) (AC)
              (struct
                type ac = AppCond.t

                type t = react

                type label = float

                let name r = r.name

                let lhs r = r.rdx

                let rhs r = r.rct

                let conds r = r.conds

                let l r = r.rate

                let map r = r.eta

                let merge_occ (b, rho, r) (_, rho', r') =
                  (b, rho +. rho', r @ r')

                let equal r r' =
                  S.equal r.rdx r'.rdx && S.equal r.rct r'.rct
                  && Base.opt_equal Fun.equal r.eta r'.eta
                  && r.rate = r'.rate

                let val_chk r = r.rate > 0.0

                let val_chk_error_msg = "Not a stochastic rate"

                let string_of_label = Printf.sprintf "%-3g"

                let make ~name ~lhs ~rhs ?conds:(c = []) r eta =
                  { name; rdx = lhs; rct = rhs; eta; rate = r; conds = c }

                let step_post x = x

                let random_step_post (ss, m) =
                  (* Sort transitions by rate *)
                  let ss_sorted =
                    List.fast_sort
                      (fun (_, a, _) (_, b, _) ->
                        compare (a : float) (b : float))
                      ss
                  in
                  (* Compute exit rate *)
                  let a0 =
                    List.fold_left (fun acc (_, rho, _) -> acc +. rho) 0.0 ss
                  in
                  let r = Random.float 1.0 *. a0 in
                  let rec aux acc = function
                    | (s, rho, reaction_rules) :: ss ->
                        let acc' = acc +. rho in
                        if acc' > r then
                          let tau =
                            1. /. a0 *. log (1. /. Random.float 1.0)
                          in
                          (Some (s, tau, reaction_rules), m)
                        else aux acc' ss
                    | [] -> (None, m)
                  in
                  aux 0.0 ss_sorted
              end)

    let is_inst r = l r = infinity
  end

  (* Priorities *)
  module P =
    Priority.Make (S) (R)
      (struct
        let f_val rr = not (List.exists R.is_inst rr)

        let f_r_val = List.for_all R.is_inst
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
    type t = float

    type l = R.label

    let init = 0.0

    let increment t l = t +. l

    let is_greater = ( > )

    let to_string = Printf.sprintf "%.4g"
  end

  module K = struct
    let typ = Rs.SBRS
  end

  include Rs.Make (S) (R) (P) ((L : Rs.L with type l = R.label))
            ((
            G : Rs.G with type l = R.label ))
            (K)

  let rate = label
end
