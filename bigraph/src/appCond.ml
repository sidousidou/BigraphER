type loc = Ctx | Param

type t = { neg : bool; where : loc; pred : Big.t }

module type C = sig
  val check_cond : t -> ctx:Big.t -> param:Big.t -> bool

  val check_cond_memo :
    t ->
    ctx:Big.t ->
    ctx_trans:Sparse.t ->
    param:Big.t ->
    param_trans:Sparse.t ->
    bool
end

module Make (S : Solver.M) : C = struct
  (* Given a context and parameter check a given application condition is
     true *)
  let check_cond_memo cnd ~ctx ~ctx_trans ~param ~param_trans =
    match cnd.where with
    | Ctx ->
        let found = S.Memo.occurs ~target:ctx ~pattern:cnd.pred ctx_trans in
        if cnd.neg then not found else found
    | Param ->
        let found =
          S.Memo.occurs ~target:param ~pattern:cnd.pred param_trans
        in
        if cnd.neg then not found else found

  let check_cond cnd ~ctx ~param =
    check_cond_memo cnd ~ctx ~ctx_trans:(Place.trans ctx.p) ~param
      ~param_trans:(Place.trans param.p)
end
