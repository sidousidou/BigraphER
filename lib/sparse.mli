(** This module provides operations on boolean matrices
    @author Michele Sevegnani *)

(** The type of boolean matrices. *)    
type bmatrix =
  { r: int;
    c: int;
    r_major: (int, int) Hashtbl.t; (* Row-major order: i -> j,...,j' *)
    c_major: (int, int) Hashtbl.t; (* Column-major order: j -> i,...,i' *)
  }

(** [make r c] build a matrix with [r] rows and [c] columns. *) 
val make : int -> int -> bmatrix

val copy : bmatrix -> bmatrix

val ( = ) : bmatrix -> bmatrix -> bool

val compare : bmatrix -> bmatrix -> int

(*val is_0 : bmatrix -> bool*)

(** String representation. 0 = false and 1 = true *)
val to_string : bmatrix -> string

val row_1 : int -> bmatrix

val row_0 : int -> bmatrix

val col_1 : int -> bmatrix

val col_0 : int -> bmatrix

val diag : int -> bmatrix

val tens : bmatrix -> bmatrix -> bmatrix

val append : bmatrix -> bmatrix -> bmatrix

val stack : bmatrix -> bmatrix -> bmatrix

val apply_iso_rows : int Iso.t -> bmatrix -> bmatrix

val apply_iso_cols : int Iso.t -> bmatrix -> bmatrix

val apply_iso : int Iso.t -> bmatrix -> bmatrix

val parse_vector : int list list -> int -> bmatrix

val chl : bmatrix -> int -> int list

val prn : bmatrix -> int -> int list

val dom : bmatrix -> IntSet.t

val leaves : bmatrix -> IntSet.t

val codom : bmatrix -> IntSet.t

val orphans : bmatrix -> IntSet.t

val siblings : bmatrix -> int -> IntSet.t

val siblings_chk: bmatrix -> bool

val partners : bmatrix -> int -> IntSet.t

val partners_chk : bmatrix -> bool

val mul : bmatrix -> bmatrix -> bmatrix

val trans : bmatrix -> bmatrix

val iter : (int -> int -> unit) -> bmatrix -> unit

val fold : (int -> int -> 'a -> 'a) -> bmatrix -> 'a -> 'a

val add : bmatrix -> int -> int -> unit

val add_list : bmatrix -> (int * int) list -> unit

val entries : bmatrix -> int

val levels : bmatrix -> IntSet.t list
