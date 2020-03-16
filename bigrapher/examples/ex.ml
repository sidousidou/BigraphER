let () =
  let a0 =
    Big.par 
      (Big.nest
         (Big.ion (Link.parse_face ["a"]) (Base.Ctrl.Ctrl ("A", 1)))
         (Big.ion (Link.parse_face ["a"; "v_a"]) (Base.Ctrl.Ctrl ("Snd", 2)))
      ) 
      (Big.nest 
         (Big.ion (Link.Face.empty) (Base.Ctrl.Ctrl ("Ready", 0))) 
         (Big.nest 
            (Big.ion (Link.Face.empty) (Base.Ctrl.Ctrl ("Fun", 0))) 
          Big.one
         )
      ) in
  print_endline ("bigraph a0:\n" ^ (Big.to_string a0))
