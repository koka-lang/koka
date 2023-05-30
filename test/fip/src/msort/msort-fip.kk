// Haskell's Data.List.sort function ported to Koka
import std/num/int32
import std/os/env

alias elem = int32

ref type pad
  Pad

type unit2
  Unit2(a : pad, b : pad)

type pair<a>
  Pair(a : a, b : a)

type sublist<a>
  SCons(a : a, cs : sublist<a>)
  STuple(a : a, b : a)

type partition<a>
  Sublist(c : sublist<a>, z : partition<a>)
  Singleton(c : a, z : partition<a>)
  End

fip fun reverse-go(c : sublist<a>, acc : sublist<a>, u : unit2) : sublist<a>
  match c
    SCons(a, cs) -> reverse-go(cs, SCons(a, acc), u)
    STuple(a, b) -> SCons(b, SCons(a, acc))

fip fun reverse-sublist(c : sublist<a>) : sublist<a>
  match c
    SCons(a, SCons(b, c)) -> reverse-go(c, STuple(b, a), Unit2(Pad,Pad))
    SCons(a, STuple(b, c)) -> SCons(c, STuple(b, a))
    STuple(a, b) -> STuple(b, a)

fip fun sequences(xs : list<elem>) : div partition<elem>
  match(xs)
    Cons(a, Cons(b, xs1)) -> if(a > b)
      then
        val (sublist, bs) = descending(b, STuple(b, a), xs1)
        Sublist(sublist, sequences(bs))
      else
        val (sublist, bs) = ascending(b, STuple(b, a), xs1)
        Sublist(sublist, sequences(bs))
    Cons(a, Nil) -> Singleton(a, End)
    Nil          -> End

fip fun descending(a : elem, sublist : sublist<elem>, bs : list<elem>) : (sublist<elem>, list<elem>)
  match(bs)
    Cons(b, bs1) | a > b -> descending(b, SCons(b, sublist), bs1)
    bs                   -> (sublist, bs)

fip fun ascending(a : elem, sublist : sublist<elem>, bs : list<elem>) : (sublist<elem>, list<elem>)
  match(bs)
    Cons(b, bs1) | (a <= b) -> ascending(b, SCons(b, sublist), bs1)
    bs                      -> (reverse-sublist(sublist), bs)

fip fun to-list(c : sublist<a>, u : unit2) : list<a>
  match c
    SCons(a, cs) -> Cons(a, to-list(cs, u))
    STuple(a, b) -> Cons(a, Cons(b, Nil))

fip fun merge-all(xs : partition<elem>) : <div> list<elem>
  match(xs)
    Sublist(x, End)   -> to-list(x, Unit2(Pad,Pad))
    Singleton(x, End) -> Cons(x, Nil)
    xs                -> merge-all(merge-pairs(xs))

fip fun merge-pairs(xs : partition<elem>) : <div> partition<elem>
  match(xs)
    Sublist(a, Sublist(b, xs1)) -> Sublist(merge(a, b, Unit2(Pad,Pad)), merge-pairs(xs1))
    Sublist(a, Singleton(b, xs1)) -> Sublist(merge-last-left(a, b, Unit2(Pad,Pad)), merge-pairs(xs1))
    Singleton(a, Sublist(b, xs1)) -> Sublist(merge-last-right(a, b, Unit2(Pad,Pad)), merge-pairs(xs1))
    Singleton(a, Singleton(b, xs1)) ->
      Sublist(if a <= b then STuple(a, b) else STuple(b, a), merge-pairs(xs1))
    xs -> xs

fip fun merge(c1 : sublist<elem>, c2 : sublist<elem>, u : unit2) : <div> sublist<elem>
  match c1
    SCons(a, cs1) -> match c2
      SCons(b, cs2) -> 
        if a <= b then SCons(a, merge(cs1, SCons(b, cs2), u))
                  else SCons(b, merge(SCons(a, cs1), cs2, u))
      STuple(b, c) ->
        if a <= b then SCons(a, merge(cs1, STuple(b, c), u))
                  else SCons(b, merge-last-left(SCons(a, cs1), c, u))
    STuple(a, b) -> match c2
      SCons(c, cs2) ->
        if a <= c then SCons(a, merge-last-right(b, SCons(c, cs2), u))
                  else SCons(c, merge(STuple(a, b), cs2, u))
      STuple(c, d) ->
        if a <= c then SCons(a, merge-right(b, Pair(c, d), u))
                  else SCons(c, merge-left(Pair(a, b), d, u))

fip fun merge-last-right(a : elem, c2 : sublist<elem>, u : unit2) : sublist<elem>
  match c2
    SCons(b, cs2) -> if a <= b then SCons(a, SCons(b, cs2))
                               else SCons(b, merge-last-right(a, cs2, u))
    STuple(b, c)  -> merge-right(a, Pair(b, c), u)

fip fun merge-last-left(c2 : sublist<elem>, d : elem, u : unit2) : sublist<elem>
  match c2
    SCons(a, cs2) -> if a <= d then SCons(a, merge-last-left(cs2, d, u))
                              else SCons(d, SCons(a, cs2))
    STuple(a, b)  -> merge-left(Pair(a, b), d, u)

fip fun merge-right(a : elem, p : pair<elem>, u : unit2) : sublist<elem>
  match p
    Pair(b, c) -> if a <= b then SCons(a, STuple(b, c))
                            else SCons(b, if a <= c then STuple(a, c) else STuple(c, a))

fip fun merge-left(p : pair<elem>, d : elem, u : unit2) : sublist<elem>
  match p
    Pair(a, b) -> if a <= d then SCons(a, if b <= d then STuple(b, d) else STuple(d, b))
                            else SCons(d, STuple(a, b))

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
