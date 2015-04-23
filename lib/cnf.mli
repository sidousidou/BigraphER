(** This module provides operations for boolean formulae. It includes functions
    for conversion to {{: https://en.wikipedia.org/wiki/Conjunctive_normal_form
    } CNF}, optimisation via auxiliary variables and integration with {{:
    http://www.dcs.gla.ac.uk/~michele/camlminisat.html } MiniSat}.
    @author Michele Sevegnani *)

(** The type of literal (as index). *)
type lit =
  | M_lit of int * int   (** Literal stored in a matrix *)
  | V_lit of int         (** Literal stored in a vector *)

(** The type of literals. *)	       
type var = 
  | P_var of lit         (** Positive literal *)
  | N_var of lit         (** Negative literal *)

(** A disjunction of variables *)
type clause = var list

(** Binary disjunction *)
type b_clause = var * var

(** The type of formulae obtained by application of the {{:
    https://en.wikipedia.org/wiki/Conjunctive_normal_form#Conversion_into_CNF
    }Tseitin transformation}. *)
type tseitin_clause =
  | Conj of clause list            (** A conjunction of clauses when  
                                       the transformation is not applied *)
  | Enc of clause * b_clause list  (** A clause of auxiliary variables and a 
                                       list binary disjunctions *)	   

(** {6 Manipulation of boolean formulae} *)
			     
(** Return the string representation of a clause. For example: ["1 V 2 V 3"]. *)
val string_of_clause : clause -> string
				   
(** [to_ij v] unboxes a positive variable stored in a matrix.  
    @raise Assert_failure when [v] is a negative literal or if it is stored as a
    vector. *)
val to_ij : var -> int * int

(** [at_most l] return the boolean encoding of the constraint {e "at most a
    literal in [l] must be [true]"}. For example, [at_most [1;2;3]] returns the
    following clause:["(1,2) or (1,3) or (2,3)"]. In general, if [l] has length [n],
    [at_most l] returns a list with [(n-1)n/2] pairs. *)
val at_most : lit list -> b_clause list

(** [at_least l] returns the boolean encoding of the constraint {e "at least a
    literal in [l] is [true]"}. This is simply a disjunction over the elements of
    [l].*)
val at_least : lit list -> clause

(** [block_rows rows c] returns the boolean encoding of the constraint {e "any
    literal in the form [(i,j)] must be [false]"} (with [i] an element of [rows]
    and [0<j<c]). For example [block_rows [1;4] 2] returns the following
    conjunction of clauses: ["!(1,0) and !(1,1) and !(4,0) and !(4,1)"]. Note
    that each clause consists of only one negated literal. *)
val block_rows : int list -> int -> clause list

(** Block a list of pairs. This is a conjunction of clauses formed by one negated
    literal (stored in a matrix). *)
val blocking_pairs : (int * int) list -> clause list

(** [tseitin l] applies {{:
    https://en.wikipedia.org/wiki/Conjunctive_normal_form#Conversion_into_CNF
    }Tseitin transformation} to [l].  

    @param l a list of pairs interpreted as ["(X1 and Y1) or (X2 and Y2) or
             ... or (Xn and Yn)"]
   
    @return A clause in the form ["(Z1 or Z2 or ... or Zn)"] for the auxiliary
            variables, and a conjunction of binary clauses: ["(!Z1 or X1) and
            (!Z1 or Y1) and ... (!Zn or Xn) and (!Zn or Yn)"].  Note, the
            encoding is not applied ({e i.e.} the result is in the form
            {!const:Cnf.tseitin_clause.Conj}) if the input list has length less
            than three. *)
val tseitin : (lit * lit) list -> tseitin_clause

(** [impl x l] returns the CNF encoding of boolean implications.
  
    @param x is the left-hand side of the implication.
  
    @param l is the right-hand side of the implication: ["(clause0 and clause1
           and ...)"]

    @return a conjunction of binary disjunctions: ["(!X or clause0) and (!X or
            clause1) and ... "]. *)
val impl : lit -> lit list list -> clause list 

(** [equiv m clauses] returns the CNF encoding of {e if and only if} boolean formulae.
    
    @param m is the left-hand side of the formula
    
    @param clauses is a conjunction of clauses ["(X0 or X1 or ...) and (Y0 or Y1
           or ...) and ..."]
    
    @return a conjunction of binary disjunctions ["(M or !X0) and (M or !X1) and
            ... (M or !Y0)"] and a conjunction of clauses ["(!M or X0 or X1 or
            ...) and (!M or Y0 or Y1 or ...) and ..."]. *)
val equiv : lit -> lit list list -> b_clause list * clause list 

(** {6 Commander-variable Encoding} *)

(** The following type structures and functions implement the encoding defined
    in this {{:
    http://www.cs.cmu.edu/~wklieber/papers/2007_efficient-cnf-encoding-for-selecting-1.pdf
    } paper}. *)
							   
(** A group in the encoding corresponds to a list of literals. *)
type group = lit list
		 
(** N-ary tree for the encoding of the auxiliary variables introduced by the encoding. *)	   
type cmd_tree = 
  | Leaf of group
  | Node of (lit * cmd_tree) list 

(** The type of boolean constraints for the encoding *)			       
type cmd_constraint =
  | Cmd_at_most of b_clause list * clause list * b_clause list
  | Cmd_exactly of b_clause list * clause list * b_clause list * clause

(** [cmd_init l n g] returns a tree of auxiliary variables. Arguments [n] and
    [g] specify the recursion threshold and the maximum group size,
    respectively. Typical values are 6 and 3. *)
val cmd_init : group -> int -> int -> cmd_tree

(** Return encoding of constraint {e "at most a literal in the input tree is
    [true]}. *)
val at_most_cmd : cmd_tree -> cmd_constraint

(** Return encoding of constraint {e "at least a literal in the input tree is
   [true]}. *)
val at_least_cmd : cmd_tree -> clause

(** Return encoding of constraint {e "exactly one literal in the input tree is
    [true]"}. *)
val exactly_one_cmd : cmd_tree -> cmd_constraint

(** Block the roots of a tree. *)
val block_cmd : int list -> clause list

(** {6 Higher level functions} *)

(** The type of a commander-variable constraint. *)				   
type cmd = {
  length : int;                 (** Number of auxiliary commander variables *)
  roots : int list;             (** Root commander variables *)
  cmd : cmd_constraint array;   (** Constraints *) 
}

(** [bijection n m t g] generates constraints for a bijection from [n] to [m]
    expressed as a matrix of assignments. Parameters [t] and [g] are used to
    configure the commander-variable encoding.
    @return a pair in which the first element encodes constraint {e "exactly one
    [true] in every row"} while the second elements encodes constraint {e "at
    most one [true] in every column"}. *)
val bijection : int -> int -> int -> int -> (cmd * cmd)

(** Similar to {!Cnf.bijection} but generates constraints for a total,
    non-surjective function from [n] to [m]. *)
val tot_fun : int -> int -> int -> int -> cmd

(** Similar to {!Cnf.bijection} but generates constraints for a one to one
    function from [n] to [n]. *)
val one_to_one : int -> int -> int -> 
  (cmd_constraint list * int) * (cmd_constraint list * int)

(** {6 Integration with Minisat} *)

(** Initialise a vector of (auxiliary) variables. *)
val init_aux_v : int -> Minisat.solver -> Minisat.var array 

(** Initialise a matrix of variables. *)
val init_aux_m : int -> int -> Minisat.solver -> Minisat.var array array

(** Post conjunction of clauses to solver. All variables refer to the same
    vector. *)
val post_conj_v : clause list -> Minisat.solver -> Minisat.var array -> unit

(** Post conjunction of clauses to solver. All variables refer to the same
    matrix.*)
val post_conj_m : clause list -> Minisat.solver -> Minisat.var array array -> unit

(** Post Tseitin constraints to solver and return array of auxiliary 
    variables. *)
val post_tseitin : clause * b_clause list -> Minisat.solver -> 
		   Minisat.var array array -> Minisat.var array

(** Post implication constraints to solver. Left hand-sides are stored in
    matrix w. *)
val post_impl : clause list -> Minisat.solver ->
		Minisat.var array array -> Minisat.var array array -> unit

(** Post equivalence constraints to solver. Left hand-sides are stored in matrix
    w. *)
val post_equiv : b_clause list * clause list -> Minisat.solver ->
		 Minisat.var array array -> Minisat.var array array -> unit

(** Post bijection constraints to solver and return auxiliary variables. *)
val post_bij : (cmd * cmd) -> Minisat.solver -> Minisat.var array array ->
	       (Minisat.var array array * int list) * (Minisat.var array array * int list)

(** Post total non-surjective function constraints to solver and return
    auxiliary variables. *)
val post_tot : cmd -> Minisat.solver -> Minisat.var array array -> 
	       Minisat.var array array * int list

(** Post one to one function constraints to solver and return auxiliary 
    variables. *)				
val post_one_to_one : (cmd_constraint list * int) * (cmd_constraint list * int) -> 
		      Minisat.solver -> Minisat.var array array -> unit

(** Post constraints to block a commander variable row. *)
val post_block_cmd : int -> Minisat.solver -> Minisat.var array array -> 
		     int list -> unit

(** Post constraint to block a column. *)
val post_block : int -> Minisat.solver -> Minisat.var array array -> unit

(** Post implication constraints to solver. *)
val post_impl2 : var list -> var list -> Minisat.solver -> 
		 Minisat.var array array -> Minisat.var array array -> unit
