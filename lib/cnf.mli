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

(** Unbox a variable stored in a matrix. *)
val to_ij : var -> int * int

(** A disjunction of variables *)
type clause = var list

(** Binary disjunction *)
type b_clause = var * var

(** At most a literal in the input list is [true]. *)
val at_most : lit list -> b_clause list

(** At least a literal in the input list is [true]. *)
val at_least : lit list -> clause

exception TSEITIN of clause list

(** Apply Tseitin transformation to a boolean formula.
    Input :  [(X1 and Y1) or (X2 and Y2) or ... or (Xn and Yn)] 
    Output : [(Z1 or Z2 or ... or Zn) and (!Z1 or X1) and (!Z1 or Y1) and ... 
             and (!Zn or Xn) and (!Zn or Yn)] *)
val tseitin : (lit * lit) list -> clause * b_clause list

(** CNF encoding of  boolean implications.
    Input :  [M -> ((X0 or X1 or ...) and (Y0 or Y1 or ...) ...)]
    Output : [(!M or X0 or X1 or ...) and (!M or Y0 or Y1 or ...) and ...] *)
val impl : lit -> lit list list -> clause list 

(** CNF encoding of [if and only if] boolean formulae.
    Input :  [M <-> ((X0 or X1 or ...) and (Y0 or Y1 or ...) ...)]
    Output : [(M or !X0) and (M or !X1) and ... and (M or !Y0) 
             (!M or X0 or X1 or ...) and (!M or Y0 or Y1 or ...) and ...] *)
val equiv : lit -> lit list list -> b_clause list * clause list 

(** {6 Commander-variable Encoding} *)

(** Data structure (n-ary tree) for the encoding of the auxiliary variables. *)
type 'a cmd_tree = 
| Leaf of 'a list
| Node of ('a * 'a cmd_tree) list 

(** Initialisation of a tree of auxiliary variables. The two [int] arguments
    specify the recursion threshold and the maximum group size, respectively. *)
val cmd_init : lit list -> int -> int -> lit cmd_tree

(** At most a literal in the input list is [true]. *)
val at_most_cmd : lit cmd_tree -> b_clause list * clause list * b_clause list

(** At least a literal in the input list is [true]. *)
val at_least_cmd : lit cmd_tree -> clause

(** Axactly one literal in the input list is [true]. *)
val exactly_one_cmd : lit cmd_tree -> 
  b_clause list * clause list * b_clause list * clause

(** {6 Higher level functions} *)

(** Generate constraints for a bijection from n to m. Parameters t and g
    are used for configure the commander-variable encoding. Auxiliary variables
    are returned. *)
val bijection : int -> int -> int -> int ->
  ((int * (b_clause list * clause list * b_clause list * clause)) list)  * 
    ((int * (b_clause list * clause list * b_clause list)) list)

(** Generate constraints for a total, non-surjective function n to m. Parameters
    t and g  are used for configure the commander-variable encoding. Auxiliary 
    variables are returned. *)
val tot_fun : int -> int -> int -> int ->
  (int * (b_clause list * clause list * b_clause list * clause)) list

(** {6 Integration with Minisat} *)

(** Initialise a vector of (auxiliary) variables. *)
val init_aux_v : int -> Minisat.solver -> Minisat.var array 

(** Initialise a matrix of variables. *)
val init_aux_m : int -> int -> Minisat.solver -> Minisat.var array array

(** Post conjunction of clauses to solver. All variables refer to the same
    vector. *)
val post_conj_v : clause list -> Minisat.solver -> Minisat.var array -> unit

(** To be used also when TSEITIN is raised. All variables refer to the same
    matrix.*)
val post_conj_m : clause list -> Minisat.solver -> Minisat.var array array -> unit

(** Post Tseitin constraints to solver and return array of auxiliary 
    variables. *)
val post_tseitin : clause * b_clause list -> Minisat.solver -> Minisat.var array array ->
  Minisat.var array

(** Post impl constraints to solver. Left hand-sides are stored in matrix w. *)
val post_impl : clause list -> Minisat.solver ->
  Minisat.var array array -> Minisat.var array array -> unit

(** Post equiv constraints to solver. Left hand-sides are stored in matrix w. *)
val post_equiv : b_clause list * clause list -> Minisat.solver ->
  Minisat.var array array -> Minisat.var array array -> unit

(** Post bijection constraints to solver and return auxiliary variables. *)
val post_bij : ((int * (b_clause list * clause list * b_clause list * clause)) list)  * 
  ((int * (b_clause list * clause list * b_clause list)) list) -> 
  Minisat.solver -> Minisat.var array array ->
  Minisat.var array list * Minisat.var array list

(** Post total non-surjective function to solver and return auxiliary 
    variables. *)
val post_tot : (int * (b_clause list * clause list * b_clause list * clause)) list -> 
  Minisat.solver -> Minisat.var array array -> Minisat.var array list

