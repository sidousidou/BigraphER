(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on boolean matrices
    @author Michele Sevegnani
    @version 0.1 *)

(** The type of boolean matrices. *)    
type bmatrix = {
  r : int; (** Number of rows *)
  c : int; (** Number of columns *)
  m : bool array array; (** Boolean matrix *)
 }

(** [make r c] builds a matrix with [r] rows and [c] columns. All its entries
    are set to [false].*) 
val make : int -> int -> bmatrix

(** [diag n] builds a diagonal matrix of size [n]. *)
val diag : int -> bmatrix

(** [stack a b] stacks two matrices with the same number of columns vertically. 
    @raise Assert_failure when the number of columns do not match. *)
val stack : bmatrix -> bmatrix -> bmatrix

(** [append a b] appends two matrices with the same number of rows horizontally.
    @raise Assert_failure when the number of rows do not match. *) 
val append : bmatrix -> bmatrix -> bmatrix

(** Boolean matrix multiplication. 
    @raise Assert_failure  when the dimensions do not match. *)
val mul : bmatrix -> bmatrix -> bmatrix

(** [copy a] copies matrix [a]. *)
val copy : bmatrix -> bmatrix

(** String representation. 0 = false and 1 = true *)
val to_string : bmatrix -> string

(** [split m r c] splits matrix [a] in four submatrices as follows
         [h | k] 
     [m = -----]     where h has [r] rows and [c] columns.
         [c | d]                          
    @raise Assert_failure when [r] or [c] exceed the dimensions of the matrix.*)
val split : bmatrix -> int -> int -> bmatrix * bmatrix * bmatrix * bmatrix

(*val copy_row : bmatrix -> bmatrix -> int -> int -> unit
val copy_col : bmatrix -> bmatrix -> int -> int -> unit*)

(** [apply_iso i m offset] applies isomorphism [i] to [m]. The result is a 
    fresh matrix. Argument [offset] is the number of rows to skip. *)
val apply_iso : Base.Iso.t -> bmatrix -> int -> bmatrix

(** [parse_vector l i] builds a matrix starting from a list of parent (rows)
    sets. *)
val parse_vector : int list list -> int -> bmatrix

(** [get_vector m] build a list of parent (rows) sets starting from matrix
    [m]. *)
val get_vector : bmatrix -> int list list

(** [chl m i] gets the set of children (columns) of [i] (row), 
    i.e. all the [m.(i).(j) = true].
    @raise Assert_failure when [i] is not a valid row index. *)
val chl : bmatrix -> int -> Base.Int_set.t

(** [prn m j] gets the set of parents (rows) of [j] (column),
    i.e. all the [m.(i).(j) = true].
    @raise Assert_failure when [j] is not a valid column index. *)
val prn : bmatrix -> int -> Base.Int_set.t

(**/**)
