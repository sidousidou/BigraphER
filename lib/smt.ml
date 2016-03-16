open Base
open Z3

(* Default context *)       
let ctx = mk_context []
       
(* Common expressions *)		     
module E = struct				    
    let eq x y = Boolean.mk_eq ctx x y
    let distinct x = Boolean.mk_distinct ctx x
    let ite b then_ else_
      = Boolean.mk_ite ctx b then_ else_
    let true_ = Boolean.mk_true ctx
    let false_ = Boolean.mk_false ctx
    let bool b = if b then true_ else false_
    let and_ t = Boolean.mk_and ctx t
    let or_ t  = Boolean.mk_or ctx t
    let not t  = Boolean.mk_not ctx t
    let imply t1 t2 = Boolean.mk_implies ctx t1 t2
    let iff t1 t2 = Boolean.mk_iff ctx t1 t2
				     
    let ( = ) x y = eq x y 
    let ( <> ) x y = distinct [x ; y]
			      
    let ( && ) x y = and_ [ x ; y ]
    let ( || ) x y = or_ [ x ; y ]
    let ( <=> ) x y = iff x y
    let ( ==> ) x y = imply x y

    let ( @@ ) f x = Expr.mk_app ctx f x
			    
    let mk_int = Arithmetic.Integer.mk_numeral_i ctx
end

type sat =
  | Unsat of Z3.Expr.expr Lazy.t  (* Proof *)
  | Sat of Z3.Model.model Lazy.t  (* Model *)
  | Unkown of string              (* Reason *)
					
module Solver = struct
    include Z3.Solver

    type t = solver

    let make () = mk_simple_solver ctx
    let add ~solver x = add solver [x]
   
    let check ~solver l =
      match check solver l with
      | UNSATISFIABLE -> Unsat (lazy (safe (get_proof solver)))
      | UNKNOWN -> Unkown (get_reason_unknown solver)
      | SATISFIABLE -> Sat (lazy (safe (get_model solver)))
  end
    
(****************** Helper functions ******************)

let int_sort = Arithmetic.Integer.mk_sort ctx

let int_sort2 = [int_sort; int_sort]					      

let bool_sort = Boolean.mk_sort ctx

let declare_symbols =
  List.map (fun s -> Symbol.mk_string ctx s)
				
type fun_def =
  { f : FuncDecl.func_decl;
    body : Z3.Expr.expr; }
		  
let fresh_vars ~dom ~names =
  assert (List.length dom = List.length names);
  let dom' = List.rev dom in
  List.rev names
  |> List.mapi (fun i n ->
		Quantifier.mk_bound ctx i (List.nth dom' i))
  |> List.rev

let define_forall ~name ~dom ~names ~vars ~body ~patterns =
  Quantifier.mk_forall ctx
		       dom
		       names			   
		       (body vars)
		       (Some 1)
		       (patterns vars)
		       []
		       (Some (Symbol.mk_string ctx name))
		       None
  |> Quantifier.expr_of_quantifier

let define_fun ~f_name ~dom ~codom ~names ~body =
  let f = FuncDecl.mk_func_decl_s ctx f_name dom codom
  and vars = fresh_vars ~dom ~names in
  let app = E.(f @@ vars) in
  let body xs = E.(app = (body xs))
  and patterns _ = [Quantifier.mk_pattern ctx [app]] in
  define_forall ~name:("define_fun_" ^ f_name) ~dom ~names ~vars ~body ~patterns
  |> fun x -> {f; body = x;}
		       
(* Adjiacency functions over nodes *)				    
let define_adj f_name m =
  let body xs =
    E.(Sparse.fold (fun v w acc ->
		    (((List.nth xs 0) = (mk_int v))
		     && ((List.nth xs 1) = (mk_int w))) :: acc)
		   m []
       |> or_) in
  define_fun ~f_name
	     ~dom:int_sort2
	     ~codom:bool_sort
	     ~names:(declare_symbols ["i"; "j"])
	     ~body
	     
(* Degree over sites (dom ns) or roots (codom rn) *)
let define_deg f_name nodes =
  let body xs =
    E.(IntSet.fold (fun i acc ->
		    ((List.nth xs 0) = (mk_int i)) :: acc)
		   nodes []
       |> or_) in
  define_fun ~f_name
	     ~dom:[int_sort]
	     ~codom:bool_sort
	     ~names:(declare_symbols ["i"])
	     ~body

(* Degree over nodes *)
let define_deg_v f_name m nodes deg_f =
  let body xs =
    E.(IntSet.fold (fun i acc ->
		    ite ((List.nth xs 0) = (mk_int i))
			(mk_int (deg_f m i))
			acc)
		   nodes false_) in
  define_fun ~f_name
	     ~dom:[int_sort]
	     ~codom:int_sort
	     ~names:(declare_symbols ["i"])
	     ~body

(* Port arity *)
let define_port_arity f_name edges =
  let body xs =
    E.(Link.Lg.fold (fun e (i, acc) ->
		     Link.PortSet.fold (fun (w, _)) e.p

				       
		     ite ((List.nth xs 0) = (mk_int i)
			  && (List.nth xs 1) = (mk_int ))
			 (mk_int arity)    
		    )
		    edges (0, false_)
       |> snd
       |> or_) in
  define_fun ~f_name
	     ~dom:int_sort2
	     ~codom:int_sort
	     ~names:(declare_symbols ["i"; "w"])
	     ~body
	     
let isomorphic_sets p_nodes t_nodes iso =
  IntSet.fold (fun i acc ->
	       IntSet.fold (fun j acc ->
			   E.(iso @@ [mk_int i] = (mk_int j)) :: acc)
			   t_nodes []
	       |> E.or_ 
	       |> fun x -> x :: acc)
	      p_nodes []
  |> E.and_
	     
(****************** Definitions of a matching instance ******************)

(* C1: adjiacency is preserved *)
let c1 ~adj_p ~adj_t ~iso_v =
  let names = declare_symbols ["v"; "w"] in
  let vars = fresh_vars ~dom:int_sort2 ~names in
  let app xs = E.(adj_p.f @@ [List.nth xs 0;
			      List.nth xs 1]) in
  let body xs =
    E.((app xs) = (adj_t.f @@ [iso_v @@ [List.nth xs 0];
			       iso_v @@ [List.nth xs 1]]))
  and patterns xs = [Quantifier.mk_pattern ctx [app xs]] in
  define_forall ~name:"c1" ~dom:int_sort2 ~names ~vars ~body ~patterns
	     
(* C2: out degrees are compatible *)
let c2 ~deg_s_p ~deg_v_out_p ~deg_v_out_t ~iso_v =
  let names = declare_symbols ["v"] in
  let vars = fresh_vars ~dom:[int_sort] ~names in
  let app xs = E.(deg_s_p.f @@ [List.nth xs 0]) in
  let body xs =
    E.((app xs)
       || ((deg_v_out_p.f @@ [List.nth xs 0])
	   = (deg_v_out_t.f @@ [iso_v @@ [List.nth xs 0]])))
  and patterns xs = [Quantifier.mk_pattern ctx [app xs]] in
  define_forall ~name:"c2" ~dom:[int_sort] ~names ~vars ~body ~patterns

(* C3: in degrees are compatible *)
let c3 ~deg_r_p ~deg_v_in_p ~deg_v_in_t ~iso_v =
  let names = declare_symbols ["v"] in
  let vars = fresh_vars ~dom:[int_sort] ~names in
  let app xs = E.(deg_r_p.f @@ [List.nth xs 0]) in
  let body xs =
    E.((app xs)
       || ((deg_v_in_p.f @@ [List.nth xs 0])
	   = (deg_v_in_t.f @@ [iso_v @@ [List.nth xs 0]])))
  and patterns xs = [Quantifier.mk_pattern ctx [app xs]] in
  define_forall ~name:"c3" ~dom:[int_sort] ~names ~vars ~body ~patterns 

(* C4: iso is a bijection (injective and surjective) *)
let c4 ~iso =
  let names = declare_symbols ["v"; "w"] in
  let vars = fresh_vars ~dom:int_sort2 ~names in
  let body xs =
    E.(((iso @@ [List.nth xs 0]) = (iso @@ [List.nth xs 1]))
       ==> ((List.nth xs 0) = (List.nth xs 1)))
  and patterns xs = [Quantifier.mk_pattern ctx [E.(iso @@ [List.nth xs 0])];
		     Quantifier.mk_pattern ctx [E.(iso @@ [List.nth xs 1])]] in
  define_forall ~name:"c4" ~dom:[int_sort] ~names ~vars ~body ~patterns 
  		
(* C5: sites are compatible *)
(* let c5_s ~adj_p ~adj_t ~iso_v =  *)
(*   Sparse.fold_c (fun s nodes_t acc -> *)
(* 		 acc) *)
(* 		adj_t [] *)
    
(* let c5_n = () *)
	       
(* C6: roots are compatible *)

(* C7: controls are compatible *)		
let c7 ~p_nodes ~t_nodes ~iso_v =
  Nodes.fold (fun i c acc ->
	      IntSet.fold (fun j acc ->
			   (* i and j have the same control *)
			   E.((iso_v @@ [mk_int i]) = (mk_int j)) :: acc)
			  (Nodes.find_all c t_nodes) []

	      |> E.or_
	      |> fun x -> x :: acc)
	     p_nodes []
  |> E.and_

(* C8: Transitive closure *)

(* C9: Ports are isomorphic *)
let c9 ~iso_e =
  let names = declare_symbols ["i"; "j"] in
  let vars = fresh_vars ~dom:int_sort2 ~names in
  let body xs =
    E.(((iso_e @@ [List.nth xs 0]) = (List.nth xs 1))
       ==> ())
  and patterns xs = [Quantifier.mk_pattern ctx [E.(iso @@ [List.nth xs 0])];
		     Quantifier.mk_pattern ctx [E.(iso @@ [List.nth xs 1])]] in
  define_forall ~name:"c9" ~dom:[int_sort] ~names ~vars ~body ~patterns 
       
let match_instance ~t_nn ~t_ns ~t_rn ~t_nodes
		   ~p_nn ~p_ns ~p_rn ~p_nodes =
  (* Place graphs encoding *)
  let adj_p = define_adj "adj_p" p_nn
  and adj_t = define_adj "adj_t" t_nn
  and deg_s_p = define_deg "deg_s_p" (Sparse.dom p_ns)
  and deg_s_t = define_deg "deg_s_t" (Sparse.dom t_ns)
  and deg_r_p = define_deg "deg_r_p" (Sparse.codom p_rn)
  and deg_r_t = define_deg "deg_r_t" (Sparse.codom t_rn)
  and deg_v_in_p = define_deg_v "deg_v_in_p" p_nn (Sparse.codom p_nn) Sparse.in_deg
  and deg_v_in_t = define_deg_v "deg_v_in_t" t_nn (Sparse.codom t_nn) Sparse.in_deg
  and deg_v_out_p = define_deg_v "deg_v_out_p" p_nn (Sparse.dom p_nn) Sparse.out_deg
  and deg_v_out_t = define_deg_v "deg_v_out_t" t_nn (Sparse.dom t_nn) Sparse.out_deg

  (* Objectives *)
  and iso_v = FuncDecl.mk_func_decl_s ctx "iso_v" [int_sort] int_sort
  and iso_e = FuncDecl.mk_func_decl_s ctx "iso_e" [int_sort] int_sort in

  [
    (* Place graph constraints *)
    c1 ~adj_p ~adj_t ~iso_v;
    c2 ~deg_s_p ~deg_v_out_p ~deg_v_out_t ~iso_v;
    c3 ~deg_r_p ~deg_v_in_p ~deg_v_in_t ~iso_v;
    c4 ~iso:iso_v;
    c7 ~t_nodes ~p_nodes ~iso_v;

    (* Link graph constraints *)
    c4 ~iso:iso_e;
  ]

let extract_iso ~model ~f ~nodes =
  IntSet.fold (fun i acc ->
	       let exp = E.(f @@ [mk_int i]) in
	       match Model.eval model exp true with
	       | None -> assert false (* Isomorphisms must be total over p nodes *)	       
	       | Some t ->
		  try Iso.add_exn i (Arithmetic.Integer.get_int t) acc with
		  | Iso.NOT_BIJECTIVE -> assert false (* Isomorphisms must be bijective *)
		  | Error _ -> assert false) (* Codomain is over integers *) 
	      nodes Iso.empty
 
let negate_solution ~f ~iso =
  Iso.fold (fun i j acc ->
	   E.((f @@ [mk_int i]) = (mk_int j)) :: acc)
	   iso []
  |> E.and_
  |> E.not


       
(*let solver = Solver.make ctx
			 
			 Solver.add ~solver t *)
