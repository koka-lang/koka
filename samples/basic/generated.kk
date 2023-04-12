pub type round
  // Round to neareast integer, round to the even number in case of a tie
  con Half-even
  // Round to nearest integer, round towards infinity in case of a tie
  con Half-ceiling
  // Round to nearest integer, round towards negative infinity in case of a tie
  con Half-floor
  // Round to nearest integer, round towards zero in case of a tie
  con Half-truncate
  // Round to nearest integer, round away from zero in case of a tie
  con Half-away-from-zero
  // Round to the minimum integer that is larger or equal
  con Ceiling
  // Round to the maximum integer that is lower or equal
  con Floor
  // Round to the nearest integer towards zero (i.e. _truncate_)
  con Truncate
  // Round to the nearest integer away from zero, i.e. toward negative infinity for negative numbers, and positive infinity for positive numbers.
  con Away-from-zero

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
