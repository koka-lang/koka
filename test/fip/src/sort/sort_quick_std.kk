import std/num/int32
import std/os/env

alias elem = int32

fun quicksort(xs : list<elem>, res : list<elem>) : <pure> list<elem>
  match(xs)
    Cons(x, xx) ->
      val (lo, hi) = partition(x, xx)
      quicksort(lo, Cons(x, quicksort(hi, res)))
    Nil -> res

fun partition(^x : elem, ys : list<elem>)
  match(ys)
    Cons(y, yy) ->
      if(y < x) then
        val (lo, hi) = partition(x, yy)
        (Cons(y, lo), hi)
      else 
        val (lo, hi) = partition(x, yy)
        (lo, Cons(y, hi))
    Nil -> (Nil, Nil)

fun rand-list(n : int32, seed : int32) : <div> list<int32>
  val a = 22695477.int32
  val c = 1.int32
  val next = a * seed + c
  if n >= 0.int32 then Cons(next, rand-list(n - 1.int32, next))
  else Nil

fun test(n : int32) 
  val xs = rand-list(n, 13.int32)
  val x = fold-int32(0.int32, (10_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            val ys = quicksort(xs, Nil)
            acc + ys.last(0.int32)
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)