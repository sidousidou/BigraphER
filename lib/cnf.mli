type m_var = int * int
type z_var = int

(* Input :  (X1 and Y1) or (X2 and Y2) or ... or (Xn and Yn) 
   Output : (Z1 or Z2 or ... or Zn) and (!Z1 or X1) and (!Z1 or Y1) and ... 
            and (!Zn or Xn) and (!Zn or Yn) *)
val tseitin : (m_var * m_var) list -> int -> ((z_var list * (z_var * m_var) list) * int)
