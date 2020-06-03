type loc = Ctx | Param

type t = { neg : bool; where : loc; pred : Big.t }

module type C = sig
  val check_cond : t -> ctx:Big.t -> param:Big.t -> bool
end

module Make (S : Solver.M) : C = struct
  (* Given a context and parameter check a given application condition is
     true *)
  let check_cond cnd ~ctx ~param =
    match cnd.where with
    | Ctx ->
        let found = S.occurs ~target:ctx ~pattern:cnd.pred in
        if cnd.neg then not found else found
    | Param ->
        let found = S.occurs ~target:param ~pattern:cnd.pred in
        if cnd.neg then not found else found
end
