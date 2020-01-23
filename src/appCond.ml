type place =
  | Ctx
  | Param

type t = {
    neg : bool;
    where : place;
    pred : Big.t;
}
