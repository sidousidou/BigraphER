include Set.Make (struct 
		     type t = int * int
		     let compare = Base.ints_compare 
		   end)
	
