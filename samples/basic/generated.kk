type hello<e::E>
  Hello(f: () -> e ())

type even<t>
  Even(o1: odd<t>)
  Zero(t: t)

type odd<t>
  Odd(o1: even<t>)

type vvalue
  IntV(i: int)
  BoolV(b: bool)
  CharV(c: char)
  StringV(s: string)

alias primop = (vvalue) -> <console,pure> vvalue

type expr<t::V>
  Int(i: int)
  Bool(b: bool)
  Char(c: char)
  String(s: string)
  Var(s: string)
  Lam(x: string, y: expr<t>)
  App(op: expr<t>, args: list<expr<t>>)
  PrimOp(name: string, prim: primop)

fun printPrim(v: vvalue): console vvalue
  v.show.println
  v

fun main() 
  val h = Hello(fn () println("Hi!"))
  h.show.println
  f(h)()
  Even(Odd(Zero(0))).show(show).println
  val x = Lam("x", Var("x"))
  val y = App(x, [Char('a')])
  val strEq = fn(x1: string, y1: string) x1 == y1
  val strShow = fn(x1: string) x1
  (==)(x,y,strEq).show.println
  (==)(x,x,strEq).show.println
  x.show(strShow).println
  y.show(strShow).println
