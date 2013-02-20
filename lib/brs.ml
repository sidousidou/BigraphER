type react = React of Big.bg * Big.bg

type sreact = Sreact of Big.bg * Big.bg * float

exception ILLEGAL_REACTION
exception NO_REACTION

let redex (React (r, _)) = r

let reactum (React (_, r)) = r

let sredex (Sreact (r, _, _)) = r

let sreactum (Sreact (_, r, _)) = r

let rate (Sreact (_, _, r)) = r

let is_instant r =
	(rate r) = infinity

(* CHECK *)
let init_match () = Match.init ()
  
(* CHECK *)
let fin_match oc ic = Match.fin_l oc ic

let check_react r0 r1 =
	(Big.inter_equals (Big.inner r0)  (Big.inner r1)) 
	&& (Big.inter_equals (Big.outer r0) (Big.outer r1))
	&& (Big.is_epi r0) && (Big.is_mono r0)

let is_valid_react r =
	check_react (redex r) (reactum r)

let is_valid_sreact r =
	(check_react (sredex r) (sreactum r))	&&	(rate r > 0.0)

let is_enabled b r oc ic =
	Big.occurs b (redex r) oc ic

let is_enabled_s b sr oc ic =
	Big.occurs b (sredex sr) oc ic

let build_occurrence (i_n, i_e) b r0 r1 = 
	let (u, d, id) = Big.decomp b r0 i_n i_e
	in Big.comp u (Big.comp (Big.tens id r1) d)

(* Compute "all" the possible evolutions in one step. *)
let evolve b (React (r0, r1)) oc ic =
	let isos = Big.occurrences b r0 oc ic
	in List.map (fun iso ->
		build_occurrence iso b r0 r1
	) isos

(* Compute "all" the possible evolutions in one step. *)
let evolve_s b (Sreact (r0, r1, lambda)) oc ic =
	let isos = Big.occurrences b r0 oc ic
	in List.map (fun iso ->
		(build_occurrence iso b r0 r1, lambda)
	) isos														

(* compute the rate for every state produced by a reaciton rule 
 xs is the result of evolve_s *)
let rate_r xs oc ic =
	Base.count xs (fun (b, _) (c, _) -> Big.equals b c oc ic)

(* returns a single step *)
let step b (React (r0, r1)) oc ic =
	try	let iso = Big.occurrence b r0 oc ic
		in build_occurrence iso b r0 r1
	with
		| Not_found -> raise NO_REACTION

(* returns a single step *)
let step_s b (Sreact (r0, r1, lambda)) oc ic =
	try	let iso = Big.occurrence b r0 oc ic
		in (build_occurrence iso b r0 r1, lambda)
	with
		| Not_found -> raise NO_REACTION

let string_of_react r =
	let lr = Big.string_of_bg (redex r)
	and rg = Big.string_of_bg (reactum r)
	in Printf.sprintf "%s\n->\n%s" lr rg
	
let string_of_sreact r =
	let lr = Big.string_of_bg (sredex r)
	and rg = Big.string_of_bg (sreactum r)
	and lambda = string_of_float (rate r)
	in Printf.sprintf "%s\n->\n%s\n@ %s" lr rg lambda
	
