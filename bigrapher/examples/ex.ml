let () =
  let a0 =
    Big.nest
      (Big.ion (Link.parse_face ["a"]) (Ctrl.C ("A", 1)))
      (Big.nest
         (Big.ion (Link.Face.empty) (Ctrl.C ("Snd", 0)))
         (Big.par
            (Big.nest
               (Big.ion (Link.parse_face ["a"; "v_a"]) (Ctrl.C ("M", 2)))
               Big.one)
            (Big.nest
               (Big.ion (Link.Face.empty) (Ctrl.C ("Ready", 0)))
               (Big.nest
                  (Big.ion (Link.Face.empty) (Ctrl.C ("Fun",0)))
                  Big.one)))) in
  print_endline ("bigraph a0:\n" ^ (Big.to_string a0));
