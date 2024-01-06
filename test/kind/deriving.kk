// Simple struct
struct hello1
  abc: int
deriving(show)

// Function
type hello<e::E>
  Hello(f: () -> e ())
deriving(show)

// Mutually recursive types
type even<t>
  Even(o1: odd<t>)
  Zero(t: t)
deriving(show,(==))

type odd<t>
  Odd(o1: even<t>)
deriving(show,(==))

// More complex domain
type vvalue
  IntV(i: int)
  BoolV(b: bool)
  CharV(c: char)
  StringV(s: string)

alias primop = (vvalue) -> <console,pure> vvalue

type expr<t,x::E>
  Int(i: int)
  X(x: () -> x ())
  Bool(b: bool)
  Char(c: char)
  String(s: string)
  Var(s: string)
  List(l: list<t>)
  Lam(x: string, y: expr<t,x>)
  App(op: expr<t,x>, args: list<expr<t,x>>)
  PrimOp(name: string, prim: primop)
deriving(show, (==))

fun main() 
  val h = Hello(fn () println("Hi!"))
  h.hello/show.println
  f(h)()
  Even(Odd(Zero(0))).show().println
  val x = Lam("x", Var("x"))
  [0, 1, 2] == [2, 3, 4]
  val y:expr<int,<>> = App(x, [Char('a')])
  y.expr/show.println
  (x == y).println
  val strEq = fn(x1: string, y1: string) x1 == y1
  val strShow = fn(x1: string) x1
  (==)(x,y).show.println
  (==)(x,x).show.println
  x.show.println
  y.show.println