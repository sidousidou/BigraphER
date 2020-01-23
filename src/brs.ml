(* Reaction rules *)
type react =
  { name : string;
    rdx  : Big.t;                  (* Redex   --- lhs   *)
    rct  : Big.t;                  (* Reactum --- rhs   *)
    eta  : Fun.t option;           (* Instantiation map *)
    conds : AppCond.t list         (* Application conditions *)
  }

module RT = struct

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
    Big.equal r.rdx r'.rdx
    && Big.equal r.rct r'.rct
    && Base.opt_equal Fun.equal r.eta r'.eta

  let val_chk _ = true

  let val_chk_error_msg = ""

  let string_of_label _ = ""

  let parse ~name ~lhs ~rhs ?conds:(c=[]) _ eta =
    { name = name;
      rdx  = lhs;
      rct  = rhs;
      eta  = eta;
      conds = c; }

  let step b rules =
    RrType.gen_step b rules merge_occ ~lhs ~rhs ~label:l ~map ~conds

  let random_step b rules =
    (* Remove element with index i *)
    let rec aux i i' acc = function
      | [] -> assert false (*BISECT-IGNORE*)
      | x :: l -> if i = i' then (x, l @ acc)
        else aux i (i' + 1) (x :: acc) l in
    let (ss, m) = step b rules in
    match ss with
    | [] -> (None, m)
    | _ -> let (s, _) =
            aux (Random.int (List.length ss)) 0 [] ss
          in (Some s, m)
end

module R = RrType.Make (RT)

(* Priorities *)
module PT = struct
  type t = RT.t list
  let f_val _ = true
  let f_r_val _ = true
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
      preds =  S_string.of_list preds; }
  let states g = g.v
  let label g = (g.preds, g.l)
  let edges g = g.e
  let string_of_l _ = ""
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
  let typ = Rs.BRS
end

include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (T)

let parse_react_unsafe ~name ~lhs ~rhs ?conds eta =
  parse_react_unsafe ~name ~lhs ~rhs ?conds () eta (* Label is ignored *)

let parse_react ~name ~lhs ~rhs ?conds eta =
  parse_react ~name ~lhs ~rhs ?conds () eta (* Label is ignored *)
