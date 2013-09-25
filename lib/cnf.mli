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

(** This module provides operations for CNF conversion.
@author Michele Sevegnani *)

type lit =
| M_lit of int * int   (** Literal stored in a matrix *)
| V_lit of int                 (** Literal stored in a vector *)

type var = 
| P_var of lit (** Positive literal *)
| N_var of lit (** Negative literal *)

(** At most a literal in the input list is [true]. *)
val at_most : lit list -> (var * var) list

(** At least a literal in the input list is [true]. *)
val at_least : lit list -> var list

(** Apply Tseitin transformation to a boolean formula.
    Input :  [(X1 and Y1) or (X2 and Y2) or ... or (Xn and Yn)] 
    Output : [(Z1 or Z2 or ... or Zn) and (!Z1 or X1) and (!Z1 or Y1) and ... 
             and (!Zn or Xn) and (!Zn or Yn)] *)
val tseitin : (lit * lit) list -> var list * (var * var) list

(** CNF encoding of [if and only if] boolean formulae.
    Input :  [M <-> ((X0 or X1 or ...) and (Y0 or Y1 or ...) ...)]
    Output : [(M or !X0) and (M or !X1) and ... and (M or !Y0) 
             (!M or X0 or X1 or ...) and (!M or Y0 or Y1 or ...) and ...] *)
val iff : lit -> lit list list -> ((var * var) list * var list list) 

(** {6 Commander-variable Encoding} *)

(** Data structure (n-ary tree) for the encoding of the auxiliary variables. *)
type 'a cmd_tree = 
| Leaf of 'a list
| Node of ('a * 'a cmd_tree) list 

(** Initialisation of a tree of auxiliary variables. The two [int] arguments
    specify the recursion threshold and the maximum group size, respectively. *)
val cmd_init : lit list -> int -> int -> lit cmd_tree

val at_most_cmd : lit cmd_tree -> (var * var) list * var list list * (var * var) list

val at_least_cmd : lit cmd_tree -> var list

val exactly_one_cmd : lit cmd_tree -> 
  (var * var) list * var list list * (var * var) list * var list
