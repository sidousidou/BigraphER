(** This module provides operations on boolean matrices
    @author Michele Sevegnani
    @version 0.3 *)

(** The type of boolean matrices. *)    

(** The type of boolean matrices. *)    
type bmatrix =
  { r: int;
    c: int;
    r_major: (int, int) Hashtbl.t; (* Row-major order: i -> j,...,j' *)
    c_major: (int, int) Hashtbl.t; (* Column-major order: j -> i,...,i' *)
  }

(** [make r c] build a matrix with [r] rows and [c] columns. *) 
val make : int -> int -> bmatrix

(** String representation. 0 = false and 1 = true *)
val to_string : bmatrix -> string

val row_1 : int -> bmatrix

val row_0 : int -> bmatrix

val col_1 : int -> bmatrix

val col_0 : int -> bmatrix

val diag : int -> bmatrix

val tens : bmatrix -> bmatrix -> bmatrix

val apply_iso : Base.Iso.t -> bmatrix -> bmatrix

val parse_vector : int list list -> int -> bmatrix

val chl : bmatrix -> int -> int list

val prn : bmatrix -> int -> int list

(* rename to leaves *)
val zero_rows : bmatrix -> int list

(* rename to orphans *)
val zero_cols : bmatrix -> int list

val siblings : bmatrix -> int -> int list

val partners : bmatrix -> int -> int list

val mul : bmatrix -> bmatrix -> bmatrix
