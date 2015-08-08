(** This module provides an implementation of sparse boolean matrices based on
    hash tables. The module also provides operations on graphs. These are
    intended to be used when boolean matrices are interpreted as adjacency
    matrices of Directed Acyclic Graphs (DAG).
    @author Michele Sevegnani *)

(*module H : Hashtbl.S with type key = int*)
				  
(** The type of boolean matrices. Only true values are stored in the matrix as
    row-column pairs. For example adding value ["(2,1)"] means that the second
    element in the third row of the matrix is true. *)
type bmatrix =
  { r : int;                           (** Number of rows.          *)
    c : int;                           (** Number of columns.       *)
    r_major : int Base.H_int.t;                 (** Row-major order index    *)
    c_major : int Base.H_int.t;                 (** Column-major order index *)
  }

(** {6 Basic operations} *)
    
(** [make r c] returns an empty matrix with [r] rows and [c] columns. *) 
val make : int -> int -> bmatrix

(** Matrix equality. *)
val ( = ) : bmatrix -> bmatrix -> bool

(** Matrix comparison. *)
val compare : bmatrix -> bmatrix -> int

(** Return the string representation of a matrix. '0' = false and 1 = true *)
val to_string : bmatrix -> string

(** [apply_rows_exn iso m] returns a copy of matrix [m] with the rows reordered
    according to [iso]. The domain of [iso] is assumed to be [{0,...,r}] with
    [r] the number of rows of [m].
    @raise Not_found if the isomorphism is undefined. *)
val apply_rows_exn : int Iso.t -> bmatrix -> bmatrix

(** Same as {!apply_rows_exn} but on columns.
    @raise Not_found if the isomorphism is undefined. *)
val apply_cols_exn : int Iso.t -> bmatrix -> bmatrix

(** Same as {!apply_rows_exn} but on both rows and columns. The matrix is
    assumed square.
    @raise Not_found if the isomorphism is undefined. *)
val apply_exn : int Iso.t -> bmatrix -> bmatrix

(** [parse_vectors l r] parses list [l] of column vectors to a matrix with [r]
    rows and [c] columns, with [c] the length of [l] . Example: [parse_vectors
    [[0;1;2];[1;2]] 4] is parsed to the following 4x2 matrix
{v
    10
    11
    11
    00
v} *)					  
val parse_vectors : int list list -> int -> bmatrix

(** Return the domain of a matrix, that is the set of rows having at least one
    [true] element. *)
val dom : bmatrix -> IntSet.t

(** Return the codomain of a matrix, that is the set of columns having at least
    one [true] element. *)
val codom : bmatrix -> IntSet.t

(** Same as [Hashtbl.iter]. *)			 
val iter : (int -> int -> unit) -> bmatrix -> unit

(** Same as [Hashtbl.fold]. *)						
val fold : (int -> int -> 'a -> 'a) -> bmatrix -> 'a -> 'a

(** [add m i j] adds [true] element [m.(i).(j)]. Arguments [i] and [j] are
    assumed to be valid indices. *)
val add : bmatrix -> int -> int -> unit

(** Add a list of elements as in {!Sparse.add}. *)				     
val add_list : bmatrix -> (int * int) list -> unit

(** Return the number of [true] elements in a matrix. *)
val entries : bmatrix -> int
			 
(** {6 Matrix operations} *)
			     
(** [row n] returns a row vector of [n] [true] elements. *)			     
val row : int -> bmatrix

(** [col n] returns a column vector of [n] [true] elements. *)		   
val col : int -> bmatrix

(** [diag n] returns a square matrix of size [n] with all [true] elements on the
    diagonal. *)
val diag : int -> bmatrix

(** [tens a b] returns the tensor product of matrices [a] and [b]. The tensor
    product is defined according to the following schema: 
{v 
    +-----------+-----------+
    |           |           |
    |     a     |           |
    |           |           |
    +-----------+-----------+
    |           |           |
    |           |     b     |
    |           |           |
    +-----------+-----------+ v}
 *)		    
val tens : bmatrix -> bmatrix -> bmatrix

(** [append a b] appends matrix [b] to the right of matrix [a]. The two matrices
    are assumed to have the same number of rows. This operation is described by
    the diagram below:
{v 
    +-----------+-----------+
    |           |           |
    |     a     |     b     |
    |           |           |
    +-----------+-----------+
v}*)  				   
val append : bmatrix -> bmatrix -> bmatrix

(** [stack a b] stacks matrix [a] on top of matrix [b]. The two matrices are
    assumed to have the same number of columns. This operation is described by
    the following diagram:
{v
    +-----------+ 
    |           |
    |     a     |
    |           |
    +-----------+
    |           |
    |     b     |
    |           |
    +-----------+ 
v}*)				     
val stack : bmatrix -> bmatrix -> bmatrix

(** [mul a b] multiplies (row by column multiplication) matrix [a] by matrix
    [b]. The number of columns of [a] is assumed to be equal to the number of
    rows of [b]. *)
val mul : bmatrix -> bmatrix -> bmatrix

(** Transitive closure. *)
val trans : bmatrix -> bmatrix

(** {6 Graph operations} *)

(** Return a list containing the children of a node. *)			 
val chl : bmatrix -> int -> int list

(** Same as {!Sparse.chl} but return a set. *)				
val chl_set : bmatrix -> int -> IntSet.t

(** Return a list containing the parents of a node. *)				
val prn : bmatrix -> int -> int list

(** Same as {!Sparse.prn} but return a set. *)				
val prn_set : bmatrix -> int -> IntSet.t

(** Return the set of leaves of a graph. *)				
val leaves : bmatrix -> IntSet.t

(** Returns the set of nodes without parents. *)
val orphans : bmatrix -> IntSet.t

(** [siblings i m] returns the set of siblings of [i]. Two nodes are siblings if
    they share a parent. *)
val siblings : bmatrix -> int -> IntSet.t

(** [siblings_chk m] returns [false] if any two columns in [m] are siblings. 
    Note that orphans are not considered siblings. *)
val siblings_chk: bmatrix -> bool

(** Dual of {!Sparse.siblings}. *)
val partners : bmatrix -> int -> IntSet.t

(** Dual of {!Sparse.siblings_chk}. *)
val partners_chk : bmatrix -> bool

(** [levels m] returns the level decomposition of [m]. Each level is obtained by
    iteratively removing the leaves in the graph until no nodes are
    left. Argument [m] is assumed square. *)
val levels : bmatrix -> IntSet.t list

(**/**)
