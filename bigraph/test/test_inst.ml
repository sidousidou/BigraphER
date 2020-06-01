open Bigraph
module ST = CI_utils.Shippable_test
module IO = CI_utils.Io
module S = Solver.Make_SAT (Solver.MS)
module BRS = Brs.Make (S)

(* Controls *)
let a = Big.ion Link.Face.empty (Ctrl.C ("A", [], 0))

and b = Big.ion Link.Face.empty (Ctrl.C ("B", [], 0))

and c = Big.ion Link.Face.empty (Ctrl.C ("C", [], 0))

let e n =
  Big.nest (Big.ion Link.Face.empty (Ctrl.C ("E", [ Ctrl.I n ], 0))) Big.one

let e1 = e 1

and e2 = e 2

and e3 = e 3

let check (a, r, b) =
  try
    BRS.step a [ r ]
    |> (fun (s', _) -> List.hd s' |> fun (f, _, _) -> f)
    |> S.equal b
  with Big.COMP_ERROR (_, _) -> false

(* Test 1 *)
let b1 = Big.par_of_list [ Big.nest a e1; Big.nest b e2; Big.nest c e3 ]

and b1' = Big.par_of_list [ Big.nest a e1; Big.nest c e3 ]

and r1 = Big.par_of_list [ a; b; c ]

and r1' = Big.par_of_list [ a; c ]

and eta1 = Some (Fun.parse [ 0; 2 ])

let rr1 = BRS.parse_react_unsafe ~name:"" ~lhs:r1 ~rhs:r1' () eta1

(* Test 2 *)
let b2 = Big.par_of_list [ Big.nest a e1; Big.nest b e2 ]

and b2' = Big.par_of_list [ Big.nest a e2; Big.nest b e1 ]

and r2 = Big.par_of_list [ a; b ]

let r2' = r2

and eta2 = Some (Fun.parse [ 1; 0 ])

let rr2 = BRS.parse_react_unsafe ~name:"" ~lhs:r2 ~rhs:r2' () eta2

(* Test 3 *)
let b3 = Big.par_of_list [ Big.nest a e1; Big.nest b e2 ]

and b3' = Big.par_of_list [ Big.nest a Big.one; Big.nest b Big.one ]

and r3 = Big.par_of_list [ a; b ]

let r3' = b3'

and eta3 = Some (Fun.parse [])

let rr3 = BRS.parse_react_unsafe ~name:"" ~lhs:r3 ~rhs:r3' () eta3

(* Tests *)
let test = [ (b1, rr1, b1'); (b2, rr2, b2'); (b3, rr3, b3') ]

let () =
  let print_res (a, r, b) =
    ST.xml_block "system-out" []
      [
        "a:\n" ^ Big.to_string a ^ "\nr:\n" ^ BRS.string_of_react r
        ^ "\nb:\n" ^ Big.to_string b;
      ]
  in
  let testcases =
    List.mapi
      (fun i ((a, r, b) as t) ->
        let label = "test_inst" ^ string_of_int i
        and print_out = print_res (a, r, b) in
        if check t then (label, __MODULE__, print_out, [])
        else
          ( label,
            __MODULE__,
            print_out,
            [ ST.xml_block "failure" [ ("message", "Wrong rewriting") ] [] ]
          ))
      test
  in
  print_endline "OK";
  IO.mkdir Sys.argv.(1);
  ST.(write_xml (testsuite "test_inst" testcases) Sys.argv.(1) Sys.argv.(2));
  print_endline "Done!";
  exit 0
