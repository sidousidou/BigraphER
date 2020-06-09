(** This module provides an implementation of sparse Boolean matrices based
    on maps. The module also provides operations on graphs. These are
    intended to be used when Boolean matrices are interpreted as adjacency
    matrices of Directed Acyclic Graphs (DAG).

    @author Michele Sevegnani *)

type t
(** The type of Boolean matrices. Only true values are stored in the matrix
    as row-column pairs. For example adding value ["(2, 1)"] means that the
    second element in the third row of the matrix is true. *)

(** {2 Basic operations} *)

val make : int -> int -> t
(** [make r c] returns an empty matrix with [r] rows and [c] columns. *)

val equal : t -> t -> bool
(** Matrix equality. *)

val compare : t -> t -> int
(** Matrix comparison. *)

val to_string : t -> string
(** Return the string representation of a matrix. ["0" = false] and
    ["1" = true]. *)

val pp : Format.formatter -> t -> unit
(** Pretty printer *)

val apply_rows : Iso.t -> t -> t
(** [apply_rows iso m] returns matrix [m] with the rows reordered according
    to [iso]. The domain of [iso] is assumed to be [{0,...,r}] with [r] the
    number of rows of [m]. *)

val apply_cols : Iso.t -> t -> t
(** Same as {!apply_rows} but on columns. *)

val apply : Iso.t -> t -> t
(** Same as {!apply_rows} but on both rows and columns. The matrix is assumed
    square. *)

val parse_vectors : int list list -> int -> t
(** [parse_vectors l r] parses list [l] of column vectors to a matrix with
    [r] rows and [c] columns, with [c] the length of [l] . Example:
    [parse_vectors \[\[0;1;2\];\[1;2\]\] 4] is parsed to the following 4x2
    matrix

    {v
    10
    11
    11
    00
    v} *)

val parse_string : int -> int -> int -> String.t list -> (t * t) * (t * t)
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
    +-----------+-----------+
    v}

    with [a: r * n], [b: r * s], [c: n * n], and [d: n * s].

    @raise Invalid_argument when the input strings are not in the correct
    format. *)

val dom : t -> IntSet.t
(** Return the domain of a matrix, that is the set of rows having at least
    one [true] element. *)

val codom : t -> IntSet.t
(** Return the codomain of a matrix, that is the set of columns having at
    least one [true] element. *)

val iter : (int -> int -> unit) -> t -> unit
(** Same as [Map.iter]. *)

val fold : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold iterating over every pair [(i, j)] defined by the map. *)

val fold_r : (int -> IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Same as [Map.fold] over [r_major]. *)

val fold_c : (int -> IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Dual of {!Sparse.fold_r}. *)

val add : int -> int -> t -> t
(** [add i j m] adds element [(i,j)]. Arguments [i] and [j] are assumed to be
    valid indexes. *)

val add_list : t -> (int * int) list -> t
(** Add a list of elements as in {!val:Sparse.add}. *)

val entries : t -> int
(** Return the number of [true] elements in a matrix. This is equivalent to
    the number of edges in a graph. *)

val edges : t -> (int * int) list
(** Return the representation of a binary matrix as a list of edges. *)

val mem : t -> int -> int -> bool
(** [mem m i j] returns [true] if edge [(i,j)] is defined by [m]. *)

(** {2 Matrix operations} *)

val row : int -> t
(** [row n] returns a row vector of [n] [true] elements. *)

val col : int -> t
(** [col n] returns a column vector of [n] [true] elements. *)

val diag : int -> t
(** [diag n] returns a square matrix of size [n] with all [true] elements on
    the diagonal. *)

val tens : t -> t -> t
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
    +-----------+-----------+
    v} *)

val append : t -> t -> t
(** [append a b] appends matrix [b] to the right of matrix [a]. The two
    matrices are assumed to have the same number of rows. This operation is
    described by the diagram below:

    {v
    +-----------+-----------+
    |           |           |
    |     a     |     b     |
    |           |           |
    +-----------+-----------+
    v}*)

val stack : t -> t -> t
(** [stack a b] stacks matrix [a] on top of matrix [b]. The two matrices are
    assumed to have the same number of columns. This operation is described
    by the following diagram:

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

val glue : t -> t -> t -> t -> t
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
    +-----------+-----------+
    v} *)

val mul : t -> t -> t
(** [mul a b] multiplies (row by column multiplication) matrix [a] by matrix
    [b]. The number of columns of [a] is assumed to be equal to the number of
    rows of [b]. *)

val trans : t -> t
(** Transitive closure. *)

(** {2 Graph operations} *)

val chl : t -> int -> IntSet.t
(** Return the children set of a node. *)

val prn : t -> int -> IntSet.t
(** Return the parent set of a node. *)

val leaves : t -> IntSet.t
(** Return the set of leaves of a graph. *)

val orphans : t -> IntSet.t
(** Returns the set of nodes without parents. *)

val siblings : t -> int -> IntSet.t
(** [siblings i m] returns the set of siblings of [i]. Two nodes are siblings
    if they share a parent. *)

val partners : t -> int -> IntSet.t
(** Dual of {!val:Sparse.siblings}. *)

val sym : t -> t
(** [sym m] returns the symmetric closure of a graph [m]. Argument [m] is
    assumed square. *)

val descendants : t -> int -> IntSet.t
(** [descendants m i] returns the set of nodes reachable from [i] in graph
    [m]. *)

val connected_comps : t -> IntSet.t list
(** Return a list of the connected components of an undirected graph. *)

val levels : t -> IntSet.t list
(** [levels m] returns the level decomposition of [m]. Each level is obtained
    by iteratively removing the leaves in the graph until no nodes are left.
    Argument [m] is assumed square. *)

val row_eq : t -> IntSet.t -> IntSet.t
(** [row_eq m js] computes a set of rows such that the union of their
    children set is equal to [js].*)

val col_eq : t -> IntSet.t -> IntSet.t
(** Dual of {!val:Sparse.row_eq}. *)

(**/**)
