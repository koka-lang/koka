// Adapted from: https://raw.githubusercontent.com/leanprover/lean4/IFL19/tests/bench/deriv.ml
type expr
  Val(value : int)
  Var(name : string)
  Add(l : expr, r : expr)
  Mul(l : expr, r : expr)
  Pow(l : expr, r : expr)
  Ln(e : expr)


fun pown(a : int, b : int) : int
  pow(a,b)


fun add(n0 : expr, m0 : expr) : div expr
  match(n0,m0)
    (Val(n),Val(m))        -> Val(n+m)
    (Val(0),f)             -> f
    (f,Val(0))             -> f
    (f,Val(n))             -> add(Val(n),f)
    (Val(n),Add(Val(m),f)) -> add(Val(n+m),f)
    (f,Add(Val(n),g))      -> add(Val(n),add(f,g))
    (Add(f, g), h)         -> add(f,add(g,h))
    (f,g)                  -> Add(f, g)



fun mul(n0 : expr, m0 : expr) : div expr
  match (n0,m0)
    (Val(n), Val(m))         -> Val(n*m)
    (Val(0), _)              -> Val(0)
    (_, Val(0))              -> Val(0)
    (Val(1), f)              -> f
    (f, Val(1))              -> f
    (f, Val(n))              -> mul(Val(n),f)
    (Val(n), Mul(Val(m), f)) -> mul(Val(n*m),f)
    (f, Mul(Val(n), g))      -> mul(Val(n),mul(f,g))
    (Mul(f, g), h)           -> mul(f,mul(g,h))
    (f, g)                   -> Mul(f, g)



fun powr(m0 : expr, n0 : expr) : div expr
  match (m0,n0)
    (Val(m), Val(n)) -> Val(pown(m,n))
    (_,  Val(0))     -> Val(1)
    (f, Val(1))      -> f
    (Val(0), _)      -> Val(0)
    (f, g)           -> Pow(f, g)



fun ln( n : expr) : expr
  match n
    Val(1) -> Val(0)
    f      -> Ln(f)



fun d( x : string, ^e : expr) : div expr
  match e
    Val(_)     -> Val(0)
    Var(y)     -> if x == y then Val(1) else Val(0)
    Add(f, g)  -> add(d(x,f),d(x,g))
    Mul(f, g)  -> add(mul(f,d(x,g)),mul(g,d(x,f)))
    Pow(f, g)  -> mul(powr(f,g),add(mul(mul(g,d(x,f)),powr(f,Val(-1))),mul(ln(f),d(x,g))))
    Ln(f)      -> mul(d(x,f),powr(f,Val(-1)))



fun count( ^e : expr) : int
  match e
    Val(_)   -> 1
    Var(_)   -> 1
    Add(f,g) -> count(f) + count(g) // + 1
    Mul(f,g) -> count(f) + count(g) // + 1
    Pow(f,g) -> count(f) + count(g) // + 1
    Ln(f)    -> count(f) // + 1



fun nest_aux(s : int, f : (int,expr) -> <div|e> expr, n : int, x : expr ) : <div|e> expr
  if n == 0 then x else
    val y = f(s - n, x)
    nest_aux(s,f,n - 1,y)



fun nest(f : (int,expr) -> <div|e> expr, n : int, e : expr ) : <div|e> expr
  nest_aux(n,f,n,e)


fun deriv(i : int, f : expr)
  val d = d("x",f)
  println(show(i+1) ++ " count: " ++ count(d).show) // ++ ", " ++ count(f).show)
  d


fun main()
  val x = Var("x")
  val f = powr(x,x)
  nest(deriv,10,f)
  println("done")

