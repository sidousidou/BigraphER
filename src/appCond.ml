type place =
  | Ctx
  | Param

type t = {
    neg : bool;
    where : place;
    pred : Big.t;
}

(* Given a context and parameter check a given application condition is true *)
let check_cond (cnd:t) ctx param =
  match cnd.where with
  | Ctx ->
    begin
      let found = Big.occurs ~target:ctx ~pattern:cnd.pred in
      if cnd.neg then not found else found
    end
  | Param ->
    begin
      let found = Big.occurs ~target:param ~pattern:cnd.pred in
      if cnd.neg then not found else found
    end
