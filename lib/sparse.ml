type smatrix =
  { r: int;
    c: int;
    r_major: (int, int) Hashtbl.t; (* Row-major order: i -> j,...,j' *)
    c_major: (int, int) Hashtbl.t; (* Column-major order: j -> i,...,i' *)
  }

(* Create an empty matrix *)
let make rows cols =
  assert(rows >= 0);
  assert(cols >= 0);
  { r = rows;
    c = cols;
    r_major = Hashtbl.create rows;
    c_major = Hashtbl.create cols;
  }

let row_1 n =
  assert (n >= 0);
  let m = make 1 n in (* inline to speed up *)
  (* 0 -> 0,...,n-1 *)
  for i = n - 1 downto 0 do (* fold returns the columns in ascending order *)
    Hashtbl.add m.r_major 0 i
  done;
  (* 0,...,n-1 -> 0 *)
  for i = 0 to n - 1 do
    Hashtbl.add m.c_major i 0
  done;
  m 

let row_0 n =
  assert (n >= 0);
  make 1 n

let col_1 n =
  assert (n >= 0);
  let m = make n 1 in (* inline to speed up *)
  (* 0,...,n-1 -> 0 *)  
  for i = 0 to n - 1 do
    Hashtbl.add m.r_major i 0
  done;
  (* 0 -> 0,...,n-1 *)
  for i = n - 1 downto 0 do
    Hashtbl.add m.c_major 0 i
  done;
  m 

let col_0 n =
  assert (n >= 0);
  make n 1

let diag n =
  assert (n >= 0);
  let m = make n n in
  for i =0 to n - 1 do
    Hashtbl.add m.r_major i i;
    Hashtbl.add m.c_major i i;
  done;
  m
  
let chl m i =
  assert (i >= 0);
  assert (i < m.r);
  Hashtbl.find_all m.r_major i

let prn m j =
  assert (j >= 0);
  assert (j < m.c);
  Hashtbl.find_all m.c_major j
