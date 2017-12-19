(** This module provides an implementation of sparse Boolean matrices based on
    maps. The module also provides operations on graphs. These are
    intended to be used when Boolean matrices are interpreted as adjacency
    matrices of Directed Acyclic Graphs (DAG).

    @author Michele Sevegnani *)

(** The type of Boolean matrices. Only true values are stored in the matrix as
    row-column pairs. For example adding value ["(2, 1)"] means that the second
    element in the third row of the matrix is true. *)
type bmatrix

(** {3 Basic operations} *)

(** [make r c] returns an empty matrix with [r] rows and [c] columns. *) 
val make : int -> int -> bmatrix

(** Matrix equality. *)
val equal : bmatrix -> bmatrix -> bool

(** Matrix comparison. *)
val compare : bmatrix -> bmatrix -> int

(** Return the string representation of a matrix. '0' = false and 1 = true *)
val to_string : bmatrix -> string

(** [apply_rows iso m] returns matrix [m] with the rows reordered
    according to [iso]. The domain of [iso] is assumed to be [{0,...,r}] with
    [r] the number of rows of [m]. *)
val apply_rows : Iso.t -> bmatrix -> bmatrix

(** Same as {!apply_rows} but on columns. *)
val apply_cols : Iso.t -> bmatrix -> bmatrix

(** Same as {!apply_rows} but on both rows and columns. The matrix is
    assumed square. *)
val apply : Iso.t -> bmatrix -> bmatrix

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

(** [parse_string r n s rows] parses list [rows] of rows encoded as ['0''1']
    strings. The resulting matrix is split as follows: 
    {v 
    +-----------+-----------+
    |           |           |
    |     a     |     b     |
    |           |           |
    +-----------+-----------+
    |           |           |
    |     c     |     d     |
    |           |           |
    +-----------+-----------+ v}
    with [a: r * n], [b: r * s], [c: n * n], and [d: n * s].

    @raise Invalid_argument when the input strings are not in the correct format. *)
val parse_string : int -> int -> int -> String.t list ->
  (bmatrix * bmatrix) * (bmatrix * bmatrix)

(** Return the domain of a matrix, that is the set of rows having at least one
    [true] element. *)
val dom : bmatrix -> IntSet.t

(** Return the codomain of a matrix, that is the set of columns having at least
    one [true] element. *)
val codom : bmatrix -> IntSet.t

(** Same as [Map.iter]. *)			 
val iter : (int -> int -> unit) -> bmatrix -> unit

(** Fold iterating over every pair [(i, j)] defined by the map. *)
val fold : (int -> int -> 'a -> 'a) -> bmatrix -> 'a -> 'a

(** Same as [Map.fold] over [r_major]. *)					
val fold_r : (int -> IntSet.t -> 'a -> 'a) -> bmatrix -> 'a -> 'a

(** Dual of {!Sparse.fold_r}. *)      
val fold_c : (int -> IntSet.t -> 'a -> 'a) -> bmatrix -> 'a -> 'a

(** [add i j m] adds  element [(i,j)]. Arguments [i] and [j] are
    assumed to be valid indexes. *)
val add : int -> int -> bmatrix -> bmatrix

(** Add a list of elements as in {!val:Sparse.add}. *)				     
val add_list : bmatrix -> (int * int) list -> bmatrix

(** Return the number of [true] elements in a matrix. This is equivalent to the
    number of edges in a graph. *)
val entries : bmatrix -> int

(** Return the representation of a binary matrix as a list of edges. *)
val edges : bmatrix -> (int * int) list

(** {3 Matrix operations} *)

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

(** [tens a b c d] computes the matrix defined below:
    {v 
    +-----------+-----------+
    |           |           |
    |     a     |     b     |
    |           |           |
    +-----------+-----------+
    |           |           |
    |     c     |     d     |
    |           |           |
    +-----------+-----------+ v}
*)		    
val glue : bmatrix -> bmatrix -> bmatrix -> bmatrix -> bmatrix

(** [mul a b] multiplies (row by column multiplication) matrix [a] by matrix
    [b]. The number of columns of [a] is assumed to be equal to the number of
    rows of [b]. *)
val mul : bmatrix -> bmatrix -> bmatrix

(** Transitive closure. *)
val trans : bmatrix -> bmatrix

(** {3 Graph operations} *)

(** Return the children set of a node. *)			 
val chl : bmatrix -> int -> IntSet.t

(** Return the parent set of a node. *)				
val prn : bmatrix -> int -> IntSet.t

(** Return the set of leaves of a graph. *)				
val leaves : bmatrix -> IntSet.t

(** Returns the set of nodes without parents. *)
val orphans : bmatrix -> IntSet.t

(** [siblings i m] returns the set of siblings of [i]. Two nodes are siblings if
    they share a parent. *)
val siblings : bmatrix -> int -> IntSet.t

(** Dual of {!val:Sparse.siblings}. *)
val partners : bmatrix -> int -> IntSet.t

(** [levels m] returns the level decomposition of [m]. Each level is obtained by
    iteratively removing the leaves in the graph until no nodes are
    left. Argument [m] is assumed square. *)
val levels : bmatrix -> IntSet.t list

(** [row_eq m js] computes a set of rows such that the union of their children
    set is equal to [js].*)
val row_eq : bmatrix -> IntSet.t -> IntSet.t

(** Dual of {!val:Sparse.row_eq}. *)	       
val col_eq : bmatrix -> IntSet.t -> IntSet.t

(**/**)
