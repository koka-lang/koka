import std/num/int32
import std/os/env

alias elem = int32

ref type pad
  Pad

ref type unit2
  Unit2(a : pad, b : pad)

type maybe2<a>
  Nothing2
  Just2(a : a, b : pad)

type sublist<a>
  SCons(a : a, cs : sublist<a>)
  STuple(a : a, b : a)

type partition<a>
  Singleton(c : sublist<a>, bdl : partition<a>)
  Sublist(c : a, bdl : partition<a>)
  End

fip fun quicksort(xs : list<elem>) : div list<elem>
  quicksort-go(xs, End)

fip fun quicksort-go(xs : list<elem>, b : partition<elem>) : div list<elem>
  match xs
    Nil -> quicksort-app(b)
    Cons(p, xx) ->
      val (lo, hi) = split-list(p, xx, Done, b, Unit2(Pad,Pad))
      quicksort-go(lo, hi)

fip fun quicksort-sublist(xs : sublist<elem>, bdl : partition<elem>, u : unit2) : div list<elem>
  match xs
    SCons(p, xx) ->
      val (lo, hi) = split-sublist(p, xx, Done, bdl, u, Unit2(Pad,Pad))
      quicksort-go(lo, hi)
    STuple(a, b) -> 
      if a <= b
        then Cons(a, Cons(b, quicksort-app(bdl)))
        else Cons(b, Cons(a, quicksort-app(bdl)))

fip fun quicksort-app(bdl : partition<elem>) : div list<elem>
  match bdl
    End -> Nil
    Sublist(p, b) -> Cons(p,quicksort-app(b))
    Singleton(c, b) -> quicksort-sublist(c, b, Unit2(Pad,Pad))

type accum<a>
  MkLo(x : a, k : accum<a>)
  MkHi(x : a, k : accum<a>)
  Done

fip fun split-list(p : elem, xs : list<elem>, k : accum<elem>, b : partition<elem>, u : unit2)  : div (list<elem>, partition<elem>)
  match xs
    Nil -> split-app1(k, p, Nil, Nothing2, b, u)
    Cons(x, xx) ->
      if x < p 
        then split-list(p, xx, MkLo(x, k), b, u)
        else split-list(p, xx, MkHi(x, k), b, u)

fip fun split-sublist(p : elem, xs : sublist<elem>, k : accum<elem>, b : partition<elem>, u : unit2, u1 : unit2) : <div> (list<elem>, partition<elem>)
  match xs
    STuple(x, y) -> split-list(p, Cons(x, Cons(y, Nil)), k, b, u)
    SCons(x, xx) -> 
      if x < p 
        then split-sublist(p, xx, MkLo(x, k), b, u, u1)
        else split-sublist(p, xx, MkHi(x, k), b, u, u1)

fip fun split-app1(k : accum<elem>, p : elem, lo : list<elem>, hi : maybe2<elem>, b : partition<elem>, u : unit2) : <div> (list<elem>, partition<elem>)
  match k
    MkLo(x, k) -> split-app1(k, p, Cons(x, lo), hi, b, u)
    MkHi(x, k) -> match hi
      Nothing2    -> split-app1(k, p, lo, Just2(x, Pad), b, u)
      Just2(y, _) -> split-app2(k, p, lo, STuple(x,y), b, u, Unit2(Pad,Pad))
    Done -> match hi
      Just2(x, _) -> (lo, Sublist(p, Sublist(x, b)))
      Nothing2    -> (lo, Sublist(p, b))

fip fun split-app2(k : accum<elem>, p : elem, lo : list<elem>, hi : sublist<elem>, b : partition<elem>, u : unit2, u1 : unit2) : (list<elem>, partition<elem>)
  match k
    MkLo(x, k) -> split-app2(k, p, Cons(x,lo), hi, b, u, u1)
    MkHi(x, k) -> split-app2(k, p, lo, SCons(x,hi), b, u, u1)
    Done       -> (lo, Sublist(p, Singleton(hi, b)))

fun rand-list(n : int32, seed : int32) : <div> list<int32>
  val a = 22695477.int32
  val c = 1.int32
  val next = a * seed + c
  if n >= 0.int32 then Cons(next, rand-list(n - 1.int32, next))
  else Nil

fun test(n : int32) 
  val xs = rand-list(n, 13.int32)
  val x = fold-int32(0.int32, (10_000_000.int32)/(max(n,1.int32)), 0.int32) fn(i,acc)
            val ys = quicksort(xs)
            acc + ys.last(0.int32)
  println("total: " ++ x.show)

fun main()
  val n = get-args().head("").parse-int.default(100).int32
  test(n)
