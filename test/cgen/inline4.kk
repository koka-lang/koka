import std/num/int32
import std/os/env

type tree<a>
  Leaf
  Bin(l : tree<a>, a : a, r : tree<a>)

fun tree32(lo : int32, hi : int32)
  if lo > hi then Leaf
  else
    val mi = lo + (hi - lo) / 2.int32
    Bin(tree32(lo, mi - 1.int32), mi, tree32(mi + 1.int32, hi))

fun tsum32(t0 : tree<int32>)
  fun go(t, acc : int32)
    match t
      Leaf -> acc
      Bin(l, a, r) -> go(r, go(l, acc + a))
  go(t0, 0.int32)

fun compose(f,g)
  fn(x) f(g(x))

fun tmap-cps( xs : tree<a>, f : a -> e b, k : tree<b> -> e tree<b> ) : e tree<b>
  match xs
    Bin(l,x,r) -> l.tmap-cps(f, compose(k, fn(l') {
      val y = f(x)
      r.tmap-cps(f, fn(r') Bin(l', y, r'))}))
    Leaf       -> k(Leaf)

fun test(n : int32) 
  val xs = tree32(1.int32,n)
  val x = fold-int32(0.int32, (100_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            acc + xs.tmap-cps(fn(x) x.inc, id).tsum32
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)
