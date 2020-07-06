open Bigraph
module S = Solver.Make_SAT (Solver.MS)

let s5 =
  "{(0, Field:0),(1, Aura:1),(2, Aura:1),(3, Aura:1),(4, Child:1),(5, \
   Child:1),(6, Child:1),(7, Aura:1),(8, Locale:1),(9, Lion:1),(10, \
   Lion:1),(11, Area(1):1),(12, GPS(2):1),(13, GPS(3):1),(14, \
   Area(2):1),(15, Area(0):1),(16, GPS(8):1),(17, Locale:1),(18, \
   GPS(1):1),(19, GPS(0):1),(20, Childattack:1),(21, Localeattack:1),(22, \
   Lionattack:2),(23, Impalaseen:2),(24, Lion:1)}\n\
   3 25 0\n\
   1111000100000000000000000\n\
   0000000010000000010001000\n\
   0000000000010011000000000\n\
   0000000000000000000000000\n\
   0000111000000000000000000\n\
   0000111000000000000000000\n\
   0000111000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000010000\n\
   0000000001100000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000001100000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000101100000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000111\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   0000000000000000000000000\n\
   ({}, {}, {(1, 1), (4, 1), (18, 1), (24, 1)})\n\
   ({}, {}, {(2, 1), (5, 1), (9, 1), (12, 1)})\n\
   ({}, {}, {(3, 1), (6, 1), (10, 1), (13, 1)})\n\
   ({}, {}, {(7, 1), (16, 1), (20, 1), (22, 1)})\n\
   ({}, {}, {(8, 1), (11, 1)})\n\
   ({}, {}, {(14, 1), (17, 1)})\n\
   ({}, {}, {(15, 1), (21, 1)})\n\
   ({}, {}, {(19, 1), (23, 1)})\n\
   ({}, {}, {(22, 1), (23, 1)})\n" |> Big.of_string

and p =
  "{(0, Child:1),(1, Childattack:1),(2, Lion:1),(3, Lionattack:2),(4, \
   Impalaseen:2)}\n\
   3 5 0\n\
   10000\n\
   01000\n\
   00111\n\
   00000\n\
   00000\n\
   00000\n\
   00000\n\
   00000\n\
   ({}, {a}, {(0, 1), (2, 1)})\n\
   ({}, {a'}, {(1, 1), (3, 1)})\n\
   ({}, {b}, {(4, 1)})\n\
   ({}, {}, {(3, 1), (4, 1)})\n" |> Big.of_string

let () =
  print_endline @@ "Occurrences:\n"
  ^ ( S.occurrences ~target:s5 ~pattern:p
    |> List.map (fun o ->
           Solver.(
             Iso.to_string o.nodes ^ ", " ^ Iso.to_string o.edges ^ ", "
             ^ Fun.to_string o.hyper_edges))
    |> String.concat "\n" );
  exit 0