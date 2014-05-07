(******************************************************************************)
(*                                                                            *)
(*        ______ __                          __     _______ ______            *)
(*       |   __ \__|.-----.----.---.-.-----.|  |--.|    ___|   __ \           *)
(*       |   __ <  ||  _  |   _|  _  |  _  ||     ||    ___|      <           *)
(*       |______/__||___  |__| |___._|   __||__|__||_______|___|__|           *)
(*                  |_____|          |__|                                     *)
(*                                                                            *)
(*       Bigraph Evaluator & Rewriting                                        *)
(*                                                                            *)
(*                                                                            *)
(*     Copyright (c) 2010-2013, Michele Sevegnani - University of Glasgow     *)       
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on boolean matrices
    @author Michele Sevegnani *)

(** The type of boolean matrices. *)    
type bmatrix =
  { r: int;  (** Number of rows. *)
    c: int;  (** Number of columns. *)
    r_major: (int, int) Hashtbl.t; (** Row-major order matrix *)
    c_major: (int, int) Hashtbl.t; (** Column-major order matrix *)
  }

(** [make r c] builds a matrix with [r] rows and [c] columns. *) 
val make : int -> int -> bmatrix

(** Copy a matrix. *)
val copy : bmatrix -> bmatrix

(** Matrix equality. *)
val ( = ) : bmatrix -> bmatrix -> bool

(** Matrix comparison. *)
val compare : bmatrix -> bmatrix -> int

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

val apply_rows_exp : int Iso.t -> bmatrix -> bmatrix

val apply_cols_exp : int Iso.t -> bmatrix -> bmatrix

val apply_exp : int Iso.t -> bmatrix -> bmatrix

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
