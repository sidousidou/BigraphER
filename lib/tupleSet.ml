include Set.Make (struct 
		     type t = int * int
		     let compare (a,b) (c,d) = 
                       let m = a - c in
                       let n = b - d in
                       match m with
                        _ when m != 0 -> m
                       | _ -> n
		   end)
	
