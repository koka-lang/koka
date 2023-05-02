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
 
fun tsum32_go(t, acc : int32)
  match t
    Leaf -> acc
    Bin(l, a, r) -> tsum32_go(r, tsum32_go(l, acc + a))

fun tsum32(t0 : tree<int32>)
  tsum32_go(t0, 0.int32)

type accum<a,b>
  Hole
  BinR(k : accum<a,b>, x : a, r : tree<a>)
  BinL(l : tree<b>, x : b, k : accum<a,b>)

fun tmap-acc( t : tree<a>, ^f : a -> e b, k : accum<a,b>) : e tree<b>
  match t
    Leaf -> tmap-app( k, f, Leaf )
    Bin(l, x, r) -> tmap-acc( l, f, BinR(k, x, r) )

fun tmap-app( k0 : accum<a,b>, ^f : a -> e b, t : tree<b> ) : e tree<b>
  match k0
    BinR(k, x, r) -> tmap-acc( r, f, BinL( t, f(x), k ) )
    BinL(l, x, k) -> tmap-app( k, f, Bin(l, x, t) )
    Hole -> t

fun test(n : int32)
  val xs = tree32(1.int32,n)
  val x = fold-int32(0.int32, (100_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            acc + xs.tmap-acc(fn(x) x.inc, Hole).tsum32
  println("total: " ++ x.show)


fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)
