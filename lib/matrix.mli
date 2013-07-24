
(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on boolean matrices
    @author Michele Sevegnani
    @version 0.2 *)

(** The type of boolean matrices. *)    
type bmatrix =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [make r c] build a matrix with [r] rows and [c] columns. Elements
    are not initialised. *) 
val make : int -> int -> bmatrix

(** [make_0 r c] build a matrix with [r] rows and [c] columns. Elements
    are initialised to [0].*)
val make_0 : int -> int -> bmatrix

(** String representation. 0 = false and 1 = true *)
val to_string : bmatrix -> string

val row_1 : int -> bmatrix
val row_0 : int -> bmatrix
val col_1 : int -> bmatrix
val col_0 : int -> bmatrix

(** [diag n] build a diagonal matrix of size [n]. *)
val diag : int -> bmatrix

(** [stack a b] stack two matrices with the same number of columns vertically.*)
val stack : bmatrix -> bmatrix -> bmatrix

(** [append a b] appends two matrices with the same number of rows
    horizontally.*)
val append : bmatrix -> bmatrix -> bmatrix

(** Boolean matrix multiplication. *)
val mul : bmatrix -> bmatrix -> bmatrix

(*
(** [copy a] copies matrix [a]. *)
val copy : bmatrix -> bmatrix
*)

(** [split m r c] splits matrix [m] in four submatrices [h], [k], [c], [d]
 as follows
         [h | k] 
     [m = -----]     where h has [r] rows and [c] columns.
         [c | d]                          
    @raise Assert_failure when [r] or [c] exceed the dimensions of the matrix.*)
val split : bmatrix -> int -> int -> bmatrix * bmatrix * bmatrix * bmatrix

(** [apply_iso i m r] applies isomorphism [i] to place graph [m]. The result
    is a fresh matrix. Argument [r] is the number of roots. Isomorphism [iso] is
    assumed total. *)
val apply_iso : Base.Iso.t -> bmatrix -> int -> bmatrix

(** [parse_vector l i] builds a matrix starting from a list of parent (rows)
    sets. *)
val parse_vector : int list list -> int -> bmatrix

(** [get_vector m] build a list of parent (rows) sets starting from matrix
    [m]. *)
val get_vector : bmatrix -> int list list

(** [chl m i] gets the set of children (columns) of [i] (row), 
    i.e. all the [m.(i).(j) = true]. *)
val chl : bmatrix -> int -> Base.Int_set.t

(** [prn m j] gets the set of parents (rows) of [j] (column),
    i.e. all the [m.(i).(j) = true].o *)
val prn : bmatrix -> int -> Base.Int_set.t

(** [zero_rows m] gets the set of rows with all [false] elements. *)
val zero_rows : bmatrix -> Base.Int_set.t

(** [zero_cols m] gets the set of columns with all [false] elements. *)
val zero_cols : bmatrix -> Base.Int_set.t

(** Computes the trasitive closure of a square matrix. *) 
val trans : bmatrix -> bmatrix

(* Computes the reflexive-trasitive closure of a square matrix. 
val trans_ref : bmatrix -> bmatrix *)

(** Computes the set of edges in a matrix. *)
val to_iso : bmatrix -> Base.Iso.t

(**/**)
