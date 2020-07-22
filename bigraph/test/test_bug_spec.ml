open Bigraph
module S = Solver.Make_SAT (Solver.MS)

let s1 =
"{(0, R:0),(1, T:0),(2, T:0),(3, R:0),(4, M:2),(5, M:2),(6, M:2),(7, M:2),(8, ND:5)}
2 9 0
110000000
000100000
000011000
001000000
000000110
000000001
000000000
000000000
000000000
000000000
000000000
({}, {a}, {(5, 1), (6, 1), (8, 1)})
({}, {b}, {(4, 1), (5, 1), (8, 1)})
({}, {c}, {(8, 1)})
({}, {d}, {(6, 1), (7, 1), (8, 1)})
({}, {x}, {(4, 1), (7, 1), (8, 1)})"
 |> Big.of_string
and lhs =
"{(0, M:2),(1, M:2),(2, M:2),(3, M:2),(4, ND:5)}
3 5 0
11000
00110
00001
00000
00000
00000
00000
00000
({}, {a}, {(0, 1), (3, 1), (4, 1)})
({}, {b}, {(0, 1), (1, 1), (4, 1)})
({}, {c}, {(1, 1), (2, 1), (4, 1)})
({}, {d}, {(2, 1), (3, 1), (4, 1)})
({}, {x}, {(4, 1)})"
|> Big.of_string
and rhs =
"{(0, M:2),(1, M:2),(2, M:2),(3, M:2),(4, ND:5)}
3 5 0
11000
00110
00001
00000
00000
00000
00000
00000
({}, {a}, {(4, 1)})
({}, {b}, {(0, 1), (1, 1), (4, 1)})
({}, {c}, {(1, 1), (2, 1), (4, 1)})
({}, {d}, {(2, 1), (3, 1), (4, 1)})
({}, {x}, {(0, 1), (4, 1)})"
|> Big.of_string

let occ_to_triple Solver.({nodes; edges; hyper_edges;}) = (nodes, edges, hyper_edges)

let () =
  print_endline @@ "s1:\n" ^ Big.to_string s1;
  print_endline @@ "r:\n" ^ Big.to_string lhs;
  let occs = Solver.([
                        { nodes = Iso.of_list [(0, 7); (1, 6); (2, 5); (3, 4); (4, 8)];
                          edges = Iso.empty;
                          hyper_edges = Fun.of_list [(0, 4); (1, 3); (2, 0); (3, 1); (4, 2)];};

                        { nodes = Iso.of_list [(0, 5); (1, 4); (2, 7); (3, 6); (4, 8)];
                          edges = Iso.empty;
                          hyper_edges = Fun.of_list [(0, 0); (1, 1); (2, 4); (3, 3); (4, 2)];};

                        { nodes = Iso.of_list [(0, 6); (1, 7); (2, 4); (3, 5); (4, 8)];
                          edges = Iso.empty;
                          hyper_edges = Fun.of_list [(0, 0); (1, 3); (2, 4); (3, 1); (4, 2)];};

                        { nodes = Iso.of_list [(0, 4); (1, 5); (2, 6); (3, 7); (4, 8)];
                          edges = Iso.empty;
                          hyper_edges = Fun.of_list [(0, 4); (1, 1); (2, 0); (3, 3); (4, 2)];};
             ])
  in
  (* Check decompositions of isomorphic occurrences are also isomorphic *)
  List.map (fun o ->
    Big.rewrite (occ_to_triple o) ~s:s1 ~r0:lhs ~r1:lhs None) occs

  |> List.for_all (fun s -> S.equal s s1)
  |> string_of_bool
  |> print_endline;

  exit 0
