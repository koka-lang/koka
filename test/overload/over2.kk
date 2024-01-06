type expr<t, x>
  Var(s: string)
  Lam(x: string, y: list<expr<t,x>>)
  PrimX(x: x)
  PrimT(v: t)

pub fun expr/show(this : expr<a,b>, ?a/show : (a) -> <div|e> string, ?b/show: (b) -> <div|e> string): <div|e> string
  match this
    Var(s)-> "Var(s: " ++ show(s) ++ ")"
    Lam(x, y)->  "Lam(x: " ++ show(x) ++  ", y: " ++ y.show ++ ")"
    PrimX(v) ->  "PrimX(v: " ++ show(v) ++ ")"
    PrimT(v) ->  "PrimT(v: " ++ show(v) ++ ")"

// fun list/show(l: list<a>, ?showa:(a -> e string)=show): e string
//   "[" ++ l.map(showa).join(", ") ++ "]"