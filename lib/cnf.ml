(* Apply tseitin transformation to a bolean formula. Input is a list of pairs.
   Each pair encodes a conjunction. The first element of the output is a 
   disjunction of auxiliary variables. The second is a conjunctions of 
   (not z or a) (not z or b) ... *)
type m_var = int * int   (* variables stored in a matrix *)
type z_var = int         (* auxiliary variables stored in a vector *)
let tseitin l n =
     List.fold_left (fun ((acc_z, acc), (i : z_var)) ((a : m_var), (b : m_var)) ->
      ((i :: acc_z, (i, a) :: (i, b) :: acc), i + 1)) (([], []), n) l
