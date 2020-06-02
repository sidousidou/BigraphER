module type T = TsType.RS with type label = unit and type limit = int

module Make (S : Solver.M) = struct
  module AC = AppCond.Make (S)

  module R =
    RrType.Make (S) (AC)
      (struct
        type ac = AC.t

        (* Reaction rules *)
        type t = {
          name : string;
          rdx : Big.t;
          (* Redex --- lhs *)
          rct : Big.t;
          (* Reactum --- rhs *)
          eta : Fun.t option;
          (* Instantiation map *)
          conds : ac list; (* Application conditions *)
        }

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

        let step b rules =
          let open struct
            module G = RrType.Make_gen (S) (AC)
          end in
          G.gen_step b rules merge_occ ~lhs ~rhs ~label:l ~map ~conds

        let random_step b rules =
          (* Remove element with index i *)
          let rec aux i i' acc = function
            | [] -> assert false
            | x :: l ->
                if i = i' then (x, l @ acc) else aux i (i' + 1) (x :: acc) l
          in
          let ss, m = step b rules in
          match ss with
          | [] -> (None, m)
          | _ ->
              let s, _ = aux (Random.int (List.length ss)) 0 [] ss in
              (Some s, m)
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
    type t = {
      v : (int * Big.t) Base.H_int.t;
      e : (int * R.label * string) Base.H_int.t;
      l : int Base.H_predicate.t;
      preds : Base.S_predicate.t;
    }

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
  module T = struct
    let typ = Rs.BRS
  end

  include TsType.Make (S) (R) (P) ((L : TsType.L with type l = R.label))
            ((
            G : TsType.G with type l = R.label ))
            (T)
end
