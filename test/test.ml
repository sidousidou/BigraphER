(* Run some tests *)
open Big_json
open Bigraph


let c0 = Ctrl.C ("Park", [], 3)
and c1 = Ctrl.C ("Hospital", [Ctrl.I 12; Ctrl.S "Church Street"], 0)
and c2 = Ctrl.C ("Bridge", [Ctrl.F 3.6], 2)

let ctrl_test = [ c0; c1; c2 ]
                
let nodes_test =
  List.fold_left (fun n (i, c) -> Nodes.add i c n)
    Nodes.empty
    [ (0, c0) ; (1, c1); (2, c0); (3, c2) ]
    
let place_test =
  Place.parse ~regions:2 ~nodes:4 ~sites:2
    [ "110000";
      "010000";
      "001000";
      "001100";
      "000011";
      "000000" ]
    
let link_test =
  let open Link in
  List.fold_left (fun l (i, o, p) ->
    Lg.add { i = i; o = o; p = p} l)
    Lg.empty
    [ (Face.empty, Face.empty, Ports.add 0 Ports.empty);
      (Face.empty, Face.empty, Ports.add 0 Ports.empty);
      (Face.empty, Face.empty, Ports.add 0 Ports.empty
                               |> Ports.add 2
                               |> Ports.add 3);
      (Face.empty, Face.empty, Ports.add 2 Ports.empty);
      (Face.empty, parse_face ["x"], Ports.add 2 Ports.empty);
      (Face.empty, Face.empty, Ports.add 3 Ports.empty) ]

let big_test =
  let open Big in
    { p = place_test;
      n = nodes_test;
      l = link_test }
    
let r_test =
  Brs.parse_react_unsafe ~lhs:big_test ~rhs:big_test None

let pr_test =
  Pbrs.parse_react_unsafe ~lhs:big_test ~rhs:big_test 0.476 None

let sr_test =
  let eta = (Fun.of_list [ (0, 1); (1, 1) ]) in
  Sbrs.parse_react_unsafe ~lhs:big_test ~rhs:big_test 8.031 (Some eta)

let () =
  print_endline "Testing ctrl_to_json:\n";
  List.iter (fun c -> print_endline @@ ctrl_to_json ~minify:false c) ctrl_test;
  print_newline ();
  print_endline "Testing nodes_to_json:\n";
  print_endline @@ nodes_to_json ~minify:false nodes_test;
  print_newline ();
  print_endline "Testing place_to_json:\n";
  print_endline @@ place_to_json ~minify:false place_test;
  print_newline ();
  print_endline "Testing link_to_json:\n";
  print_endline @@ link_to_json ~minify:false link_test;
  print_newline ();
  print_endline "Testing big_to_json:\n";
  print_endline @@ big_to_json ~minify:false big_test;
  print_newline ();
  print_endline "Testing react_to_json:\n";
  print_endline @@ react_to_json ~minify:false r_test;
  print_newline ();
  print_endline "Testing preact_to_json:\n";
  print_endline @@ preact_to_json ~minify:false pr_test;
  print_newline ();
  print_endline "Testing sreact_to_json:\n";
  print_endline @@ sreact_to_json ~minify:false sr_test;
  print_newline ();
  
