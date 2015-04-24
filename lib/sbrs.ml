open Printf

type sreact =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    rate : float
  }
       
include RrType.Make(
	    struct
	      type t = sreact
	      type label = float 
	      type occ = Big.bg * float

	      let lhs r = r.rdx
	      let rhs r = r.rct
	      let l r = r.rate
	      let map r = r.eta
	      let string_of_label l = string_of_float l
 	      let val_chk r = r.rate > 0.0
	      let to_occ b r = (b, r.rate)
	      let big_of_occ (b, _) = b
	      let merge_occ (b, rho) (_, rho') = (b, rho +. rho')
	    end)

type ctmc = {
  v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
  (* p : (int, Bilog) Hashtbl.t Predicates *)
  e : (int, (int * float)) Hashtbl.t;
  l : (int, int) Hashtbl.t;  
}

type stats = {
  t : float;    (** Execution time *)
  sim : float;  (** Simulation time *)
  s : int;      (** Number of states *)
  r : int;      (** Number of reaction *)
  o : int;      (** Number of occurrences *)
}

type p_class = 
  | P_class of sreact list 
  | P_rclass of sreact list

(* raised when a state was already discovered *)
exception OLD of int

(* raised when the size of the ts reaches the limit *)
exception LIMIT of ctmc * stats

(* raised when a deadlock is reached in a simulation *)
exception DEAD of int

let init_ctmc n = 
  { v = Hashtbl.create n;
    e = Hashtbl.create n;
    l = Hashtbl.create n;
  }

let is_inst r =
  (l r) = infinity

let is_valid_p c =
  match c with
  | P_class rr -> (List.for_all is_valid rr)
                  && (List.length rr > 0) 
                  && (not (List.exists is_inst rr))
  | P_rclass rr -> (List.for_all is_valid rr)
                   && (List.length rr > 0) 
                   && (List.for_all is_inst rr)

let is_valid_p_l l = 
  List.exists (fun c ->
      match c with
      | P_class _ -> true
      | P_rclass _ -> false) l

let rec is_class_enabled b rs = 
    match rs with
    | [] -> false
    | r :: rs ->
      if Big.occurs b (lhs r) then true
      else is_class_enabled b rs

(* rule selection: second step of SSA 
   raise DEAD *)
let select_sreact (s : Big.bg) srules m =
  (* sort transitions by rate *)
  let (ss, m') = step s srules in
  let ss_sorted = List.fast_sort (fun a b ->
      Pervasives.compare ((snd a) : float) ((snd b) : float)) ss in
  (* compute exit rate *)
  let a0 =
    List.fold_left (fun acc (_, rho) -> acc +. rho) 0.0 ss in
  let r = (Random.float 1.0) *. a0 in
  let rec aux (l : (Big.bg * float) list) (acc : float) =
    match l with
    | (s, rho) :: ss ->
      let acc' = acc +. rho in 
      if acc' > r then begin
        let tau = (1. /. a0) *. (log (1. /. (Random.float 1.0))) in
        (s, tau) 
      end else aux ss acc'
    | [] -> raise (DEAD m)
  in (aux ss_sorted 0.0, m + m')

let is_new b v =
  let k = Big.key b in
  let k_buket = 
    Hashtbl.find_all v k in
  try 
    let (old, _) = List.find (fun (_, b') ->
        Big.equal b b') k_buket in
    raise (OLD old)
  with
  | Not_found -> true

(* Scan the piority classes and reduce a state. Stop when no more
   rules can be applied or when a non reducing piority class is
   enabled. *)
let rec rewrite (s : Big.bg) classes (m : int) =
  match classes with
  | [] -> (s, m)
  | c :: cs ->
    begin
      match c with
      | P_class rr  ->
        (* if there are matches then exit, skip otherwise *)
        if is_class_enabled s rr then (s, m)
        else rewrite s cs m
      | P_rclass rr ->
        begin
          let (s', i) = fix s rr in
          rewrite s' cs (m + i)
        end
    end 

(* Partition a list of bigraphs into new and old states *)
let _partition_aux ctmc i iter_f =
  List.fold_left (fun (new_acc, old_acc, i) (b, rho) ->
      try 
        ignore (is_new b ctmc.v);
        let i' = i + 1 in
        iter_f i' b;
        (((i', b), rho) :: new_acc, old_acc, i')
      with
      | OLD x -> (new_acc, (x, rho) :: old_acc, i)
    ) ([], [], i)

(* Iterate over priority classes *)
let rec _scan curr m ctmc i iter_f pl pl_const =
  match pl with
  | [] -> (([], [], i), m)
  | c :: cs -> (
      match c with
      | P_class rr -> (
          let (ss, l) = step curr rr in
          if l = 0 then _scan curr m ctmc i iter_f cs pl_const 
          else 
            (* apply rewriting - instantaneous *)
            let (ss', l') = 
              List.fold_left (fun (ss,  l) (s, rho) -> 
                  let (s', l') = rewrite s pl_const l in
                  ((s', rho) :: ss, l')
                ) ([], l) ss in
            (_partition_aux ctmc i iter_f ss', m + l')
        )
      | P_rclass _ -> (* skip *)
        _scan curr m ctmc i iter_f cs pl_const
    )

let rec _bfs ctmc q i m (scan_f, p_classes, t0, limit, iter_f) =
  if not (Queue.is_empty q) then
    if i > limit then begin
      let stats = {
        t = (Unix.gettimeofday ()) -. t0;
        sim = infinity; 
        s = Hashtbl.length ctmc.v;
        r = Hashtbl.length ctmc.e;
        o = m;
      } in 
      raise (LIMIT (ctmc, stats))
    end else begin 
      let (v, curr) = Queue.pop q in
      let ((new_s, old_s, i'), m') = 
        scan_f curr m ctmc i iter_f p_classes p_classes in
      (* Add new states to v *)
      let new_s' = fst (List.split new_s) in
      List.iter (fun (i, b) -> 
          Hashtbl.add ctmc.v (Big.key b) (i, b)) new_s';
      (* Add new states to q *)
      List.iter (fun s -> Queue.push s q) new_s';
      (* Add labels for new states *)
      (* TO DO *)
      (* Add edges from v to new states *)
      List.iter (fun ((u, _), rho) -> 
          Hashtbl.add ctmc.e v (u, rho)) new_s;
      (* Add edges from v to old states *)
      List.iter (fun (u, rho) -> Hashtbl.add ctmc.e v (u, rho)) old_s;
      (* recursive call *)
      _bfs ctmc q i' m' (scan_f, p_classes, t0, limit, iter_f)
    end 
  else
    let stats = {
      t = (Unix.gettimeofday ()) -. t0;
      sim = infinity; 
      s = Hashtbl.length ctmc.v;
      r = Hashtbl.length ctmc.e;
      o = m;
    } in 
    (ctmc, stats)

let _init_bfs s0 rewrite _scan srules limit ctmc_size iter_f =
  let consts = 
    (_scan, srules, Unix.gettimeofday (), limit, iter_f)
  and q = Queue.create () in
  (* apply rewriting to s0 *)
  let (s0', m) = rewrite s0 srules 0
  and ctmc = init_ctmc ctmc_size in
  Queue.push (0, s0') q;
  (* add initial state *)
  Hashtbl.add ctmc.v (Big.key s0') (0, s0');
  (ctmc, s0', q, m, consts)

let bfs s0 srules limit ctmc_size iter_f =
  let (ctmc, s0', q, m, consts) =
    _init_bfs s0 rewrite _scan srules limit ctmc_size iter_f in
  iter_f 0 s0';
  _bfs ctmc q 0 m consts    

let _init_sim s0 rewrite _scan srules t_max ctmc_size iter_f =
  Random.self_init ();
  let consts = 
    (_scan, srules, Unix.gettimeofday (), t_max, iter_f) in
  (* apply rewriting to s0 *)
  let (s0', m) = rewrite s0 srules 0
  and ctmc = init_ctmc ctmc_size in
  (* add initial state *)
  Hashtbl.add ctmc.v (Big.key s0') (0, s0');
  (ctmc, s0', m, consts)

let rec _scan_sim (s : Big.bg) (m : int) iter_f pl pl_const = 
  match pl with
  | [] -> raise (DEAD m)
  | c :: cs ->
    begin
      match c with
      | P_class rr ->
        begin
          try
            let ((s', tau), m) = select_sreact s rr m in
            let (s'', m') = rewrite s' pl_const m in
            ((s'', tau), m')
          with
          | DEAD m -> (* skip *)
            _scan_sim s m iter_f cs pl_const
        end
      | P_rclass _ -> (* skip *)
        _scan_sim s m iter_f cs pl_const
    end

let rec _sim ctmc s t i m (scan_f, srules, t0, t_max, iter_f) =
  if t < t_max 
  then
    try
      let ((s', tau), m') = 
        scan_f s m iter_f srules srules in
      iter_f (i + 1) s';
      Hashtbl.add ctmc.v (Big.key s') (i + 1, s');
      Hashtbl.add ctmc.e i (i + 1, tau);
      _sim ctmc s' (t +. tau) (i + 1) m' (scan_f, srules, t0, t_max, iter_f)
    with
    | DEAD m -> (let stats = {
        t = (Unix.gettimeofday ()) -. t0;
        sim = t; 
        s = Hashtbl.length ctmc.v;
        r = Hashtbl.length ctmc.e;
        o = m;
      } in 
       (ctmc, stats))
  else let stats = {
    t = (Unix.gettimeofday ()) -. t0;
    sim = t; 
    s = Hashtbl.length ctmc.v;
    r = Hashtbl.length ctmc.e;
    o = m;
  } in 
    raise (LIMIT (ctmc, stats))

let sim s0 srules t_max ctmc_size iter_f =
  let (ctmc, s0', m, consts) =
    _init_sim s0 rewrite _scan_sim srules t_max ctmc_size iter_f in
  iter_f 0 s0';
  _sim ctmc s0' 0.0 0 m consts    

let to_dot ctmc =
  let rank = "{ rank=source; 0 };\n" in
  let states =
    Hashtbl.fold (fun _ (i, _) buff -> 
        if i = 0 then sprintf 
            "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
             fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \
             style=\"bold\" ];\n" 
            buff i i i i
        else sprintf 
            "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
             fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n" 
            buff i i i i
      ) ctmc.v ""
  and edges =
    Hashtbl.fold (fun v (u, rho) buff -> 
        sprintf "%s%d -> %d [ label=\" %.3g\", fontname=\"monospace\", fontsize=7.0,\
                 arrowhead=\"vee\", arrowsize=0.5 ];\n" 
          buff v u rho
      ) ctmc.e "" in
  sprintf "digraph ctmc {\nstylesheet = \"style_sbrs.css\"\n%s%s\n%s}" rank states edges

let to_prism ctmc =
  let dims = 
    sprintf "%d %d\n" (Hashtbl.length ctmc.v) (Hashtbl.length ctmc.e) in
  Hashtbl.fold (fun v (u, rho) buff ->
      sprintf "%s%d %d %f\n" buff v u rho) ctmc.e dims

let iter_states f ctmc =
  Hashtbl.iter (fun _ (i, b) -> f i b) ctmc.v
