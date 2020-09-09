(* Reaction rules *)
type react = {
  name : string;
  rdx : Big.t;
  (* Redex --- lhs *)
  rct : Big.t;
  (* Reactum --- rhs *)
  eta : Fun.t option;
  (* Instantiation map *)
  conds : AppCond.t list; (* Application conditions *)
}

type graph = {
  v : (int * Big.t) Base.H_int.t;
  e : (int * unit * string) Base.H_int.t;
  (* drop unit? *)
  l : int Base.H_predicate.t;
  preds : Base.S_predicate.t;
}

module type T =
  Rs.RS
    with type react = react
     and type ac := AppCond.t
     and type label = unit
     and type graph = graph
     and type limit = int

module Make (S : Solver.M) = struct
  module AC = AppCond.Make (S)

  module R =
    React.Make (S) (AC)
      (struct
        type ac = AppCond.t

        (* Reaction rules *)
        type t = react

        type label = unit

        let name r = r.name

        let lhs r = r.rdx

        let rhs r = r.rct

        let conds r = r.conds

        let l _ = ()

        let map r = r.eta

        let merge_occ (b, _, r) (_, _, r') = (b, (), r @ r')

        let equal r r' =
          S.equal r.rdx r'.rdx && S.equal r.rct r'.rct
          && Base.opt_equal Fun.equal r.eta r'.eta

        let val_chk _ = true

        let val_chk_error_msg = ""

        let string_of_label _ = ""

        let make ~name ~lhs ~rhs ?conds:(c = []) _ eta =
          { name; rdx = lhs; rct = rhs; eta; conds = c }

        let step_post x = x

        let random_step_post (ss, m) =
          (* Remove element with index i *)
          let rec aux i i' = function
            | [] -> assert false
            | x :: l -> if i = i' then x else aux i (i' + 1) l
          in
          match ss with
          | [] -> (None, m)
          | _ -> (Some (aux (Random.int (List.length ss)) 0 ss), m)
      end)

  (* Priorities *)
  module P =
    Priority.Make (S) (R)
      (struct
        let f_val _ = true

        let f_r_val _ = true
      end)

  (* Transition system graph data structure *)
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

    let string_of_l _ = ""
  end

  (* Computation limit *)
  module L = struct
    type t = int

    type l = R.label

    let init = 0

    let increment t _ = t + 1

    let is_greater = ( > )

    let to_string = string_of_int
  end

  (* Reactive system type *)
  module K = struct
    let typ = Rs.BRS
  end

  include Rs.Make (S) (R) (P) ((L : Rs.L with type l = R.label))
            ((
            G : Rs.G with type l = R.label ))
            (K)
end
