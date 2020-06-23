open Bigraph
(* module ST = CI_utils.Shippable_test
 * module IO = CI_utils.Io *)
module S = Solver.Make_SAT (Solver.MC)

let t28 = "2 9 0 5
R M M T T M M R ND
100100000
000000010
011000000
000000000
000000000
000010000
000001100
000000000
000000000
000000001
000000000
2 7 9 t
2 3 9 t
3 6 9 t
6 7 9 t
9 t
"
|> Big.parse
and p39 = "3 5 0 5
M M M M ND
11000
00110
00001
00000
00000
00000
00000
00000
1 4 5 t
1 2 5 t
2 3 5 t
3 4 5 t
5 t
"
|> Big.parse
and p_autos = [
    (Iso.of_list [(0, 1); (1, 0); (2, 3); (3, 2); (4, 4)], Iso.empty);
    (Iso.of_list [(0, 2); (1, 3); (2, 0); (3, 1); (4, 4)], Iso.empty); (* sol mapping *)
    (Iso.of_list [(0, 3); (1, 2); (2, 1); (3, 0); (4, 4)], Iso.empty) ]
and place_isos = [
    Iso.of_list [ (0, 5); (1, 6); (2, 1); (3, 2); (4, 8) ];
    Iso.of_list [ (0, 2); (1, 1); (2, 6); (3, 5); (4, 8) ];     (* old solution *)
    Iso.of_list [ (0, 1); (1, 2); (2, 5); (3, 6); (4, 8) ];
    Iso.of_list [ (0, 6); (1, 5); (2, 2); (3, 1); (4, 8) ]      (* new solution *)
  ]
and sol_new = Solver.({
                         nodes = Iso.of_list [ (0, 6); (1, 5); (2, 2); (3, 1); (4, 8) ];
                         edges = Iso.empty;
                         hyper_edges = Fun.of_list [ (0, 0); (1, 3); (2, 2); (3, 1); (4, 4) ];
              })
and sol_old = Solver.({
                         nodes = Iso.of_list [ (0, 2); (1, 1); (2, 6); (3, 5); (4, 8) ];
                         edges = Iso.empty;
                         hyper_edges = Fun.of_list [ (0, 2); (1, 1); (2, 0); (3, 3); (4, 4) ];
              })


let () =
  Big.(
    let c, d, id =
      decomp ~target:t28 ~pattern:p39 ~i_n:sol_new.nodes ~i_e:sol_new.edges sol_new.hyper_edges in
    let f = comp (tens p39 id) d in
    let b_new = comp c f in
    let c', d', id' =
      decomp ~target:t28 ~pattern:p39 ~i_n:sol_old.nodes ~i_e:sol_old.edges sol_old.hyper_edges in
    let f' = comp (tens p39 id') d' in
    let b_old =  comp c' f' in
    (* Testing f epimorphic: c * f = c' * f' => c = c' *)
    print_endline @@ "f epi: " ^ (is_epi f |> string_of_bool);
    print_endline @@ "f' epi: " ^ (is_epi f' |> string_of_bool);
    if S.equal f f' && S.equal b_new b_old
    then
      (if S.equal c c'
       then print_endline "epi"
       else (print_endline @@ "not epi\nc:\n" ^ (to_string c)
                              ^ "\nc':\n" ^ (to_string c')))
    else print_endline "epi (false premise)";
    exit 0)
