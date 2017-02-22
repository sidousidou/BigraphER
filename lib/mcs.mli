(* A directed graph with coloured vertices and unlabelled edges.  There must be
 * no loops.  Entry .(i).(j) of the adjacency matrix takes values as follows:
 *   0 if there is no edge between i and j
 *   1 if there is only an edge i->j
 *   2 if there is only an edge j->i
 *   3 if both edges are present
 *)
type graph =
  { adjmat : int array array;
    colours : int array;
    n : int;
  }

(* Given graphs G and H and a predicate f, returns a list of mappings of maximum
 * size that satisfy f.  A mapping is a list of (u,v) pairs where
 * (1) the u are vertices in G and the v are vertices in H, and
 * (2) the subgraph of G induced by the u equals the subgraph of H induced by the v.
 *)
val mcs :
  graph -> graph -> ((int * int) list -> bool) -> (int * int) list list
