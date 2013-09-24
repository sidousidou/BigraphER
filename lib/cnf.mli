type lit =
| M_lit of int * int   (** Literal stored in a matrix *)
| V_lit of int                 (** Literal stored in a vector *)

type var = 
| P_var of lit (** Positive literal *)
| N_var of lit (** Negative literal *)

(** Input :  (X1 and Y1) or (X2 and Y2) or ... or (Xn and Yn) 
    Output : (Z1 or Z2 or ... or Zn) and (!Z1 or X1) and (!Z1 or Y1) and ... 
             and (!Zn or Xn) and (!Zn or Yn) *)
val tseitin : (var * var) list -> var list * (var * var) list

(** Input :  M <-> ((X0 or X1 or ...) and (Y0 or Y1 or ...) ...)
    Output : (M or !X0) and (M or !X1) and ... and (M or !Y0) 
             (!M or X0 or X1 or ...) and (!M or Y0 or Y1 or ...) and ... *)
val iff : var -> var list list -> ((var * var) list * var list list) 
