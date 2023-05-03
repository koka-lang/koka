// Haskell's Data.List.sort function ported to Koka
import std/num/int32
import std/os/env

alias elem = int32

fun sequences(xs : list<elem>) : <div> list<list<elem>>
  match(xs)
    Cons(a, Cons(b, xs1)) ->
      if(a > b) then descending(b, Cons(a, Nil), xs1)
      else ascending(b, Cons(a, Nil), xs1)
    _ -> Cons(xs, Nil)

fun descending(a : elem, chain : list<elem>, bs : list<elem>) : <div> list<list<elem>>
  match(bs)
    Cons(b, bs1) | a > b -> descending(b, Cons(a, chain), bs1)
    _ -> Cons(Cons(a, chain), sequences(bs))

fun ascending(a : elem, chain : list<elem>, bs : list<elem>) : <div> list<list<elem>>
  match(bs)
    Cons(b, bs1) | (a <= b) -> ascending(b, Cons(a, chain), bs1)
    _ -> Cons(reverse(Cons(a, chain)), sequences(bs))

fun merge-all(xs : list<list<elem>>) : <div> list<elem>
  match xs
    Cons(x, Nil) -> x
    _ -> merge-all(merge-pairs(xs))

fun merge-pairs(xs : list<list<elem>>) : <div> list<list<elem>>
  match xs
    Cons(a, Cons(b, xx)) -> Cons(merge(a, b), merge-pairs(xx))
    _ -> xs

fun merge(xs : list<elem>, ys : list<elem>) : <div> list<elem>
  match(xs, ys)
    (Cons(x, xx), Cons(y, yy)) ->
      if(x > y) then Cons(y, merge(xs, yy))
      else Cons(x, merge(xx, ys))
    (Cons(_, _), Nil) -> xs
    (_, _) -> ys

fun rand-list(n : int32, seed : int32) : <div> list<int32>
  val a = 22695477.int32
  val c = 1.int32
  val next = a * seed + c
  if n >= 0.int32 then Cons(next, rand-list(n - 1.int32, next))
  else Nil

fun test(n : int32) 
  val xs = rand-list(n, 13.int32)
  val x = fold-int32(0.int32, (10_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            val ys = merge-all(sequences(xs))
            acc + ys.last(0.int32)
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)