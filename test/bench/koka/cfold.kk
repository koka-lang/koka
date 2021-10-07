// Adapted from https://github.com/leanprover/lean4/blob/IFL19/tests/bench/const_fold.hs
type expr
  con Var( : int )
  con Val( : int )
  con Add( l : expr, r : expr )
  con Mul( l : expr, r : expr )


fun mk_expr( n : int, v : int ) : div expr
  if n==0
   then (if v==0 then Var(1) else Val(v))
   else Add( mk_expr(n - 1, v+1), mk_expr(n - 1, max(v - 1,0)) )


fun append_add( e0 : expr, e3 : expr ) : expr
  match e0
    Add(e1,e2) -> Add(e1, append_add(e2,e3))
    _          -> Add(e0,e3)



fun append_mul( e0 : expr, e3 : expr ) : expr
  match e0
    Mul(e1,e2) -> Mul(e1, append_mul(e2,e3))
    _          -> Mul(e0,e3)



fun reassoc( e : expr ) : expr
  match e
    Add(e1,e2) -> append_add(reassoc(e1), reassoc(e2))
    Mul(e1,e2) -> append_mul(reassoc(e1), reassoc(e2))
    _ -> e



fun cfold( e : expr ) : expr
  match e
    Add(e1,e2) ->
      val e1' = cfold(e1)
      val e2' = cfold(e2)
      match e1'
        Val(a) -> match e2'
          Val(b)        -> Val(a+b)
          Add(f,Val(b)) -> Add(Val(a+b),f)
          Add(Val(b),f) -> Add(Val(a+b),f)
          _             -> Add(e1',e2')
        _ -> Add(e1',e2')
    Mul(e1,e2) ->
      val e1' = cfold(e1)
      val e2' = cfold(e2)
      match e1'
        Val(a) -> match e2'
          Val(b)        -> Val(a*b)
          Mul(f,Val(b)) -> Mul(Val(a*b),f)
          Mul(Val(b),f) -> Mul(Val(a*b),f)
          _             -> Mul(e1',e2')
        _ -> Mul(e1',e2')
    _ -> e


fun eval(e : expr) : int
  match e
    Var      -> 0
    Val(v)   -> v
    Add(l,r) -> eval(l) + eval(r)
    Mul(l,r) -> eval(l) * eval(r)


fun main() : <div,console> ()
  val e = mk_expr(20,1)
  val v1 = eval(e)
  val v2 = e.reassoc.cfold.eval
  println( v1.show )
  println( v2.show )

