// Haskell's Data.List.sort function ported to Koka
import std/num/int32
import std/os/env

alias elem = int32

ref type pad
  Pad

type unit2
  Unit2(a : pad, b : pad)

type sublist<a>
  SCons(a : a, cs : sublist<a>)
  STuple(a : a, b : a)

type partition<a>
  Singleton(c : sublist<a>, z : partition<a>)
  Sublist(c : a, z : partition<a>)
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

fip fun to-list(c : sublist<a>, u : unit2) : list<a>
  match c
    SCons(a, cs) -> Cons(a, to-list(cs, u))
    STuple(a, b) -> Cons(a, Cons(b, Nil))

fip fun sequences(xs : list<elem>) : div partition<elem>
  match(xs)
    Cons(a, Cons(b, xs1)) ->
      if(a > b) then descending(b, STuple(b, a), xs1, Unit2(Pad,Pad))
                else ascending(b, STuple(b, a), xs1, Unit2(Pad,Pad))
    Cons(a, Nil) -> Sublist(a, End)
    Nil          -> End

fip fun descending(a : elem, sublist : sublist<elem>, bs : list<elem>, u : unit2) : div partition<elem>
  match(bs)
    Cons(b, bs1) | a > b -> descending(b, SCons(b, sublist), bs1, u)
    bs                   -> Singleton(sublist, sequences(bs))

fip fun ascending(a : elem, sublist : sublist<elem>, bs : list<elem>, u : unit2) : div partition<elem>
  match(bs)
    Cons(b, bs1) | (a <= b) -> ascending(b, SCons(b, sublist), bs1, u)
    bs                      -> Singleton(reverse-sublist(sublist), sequences(bs))

fip fun merge-all(xs : partition<elem>) : div list<elem>
  match(xs)
    Singleton(x, End) -> to-list(x, Unit2(Pad,Pad))
    Sublist(x, End)   -> Cons(x, Nil)
    xs                -> merge-all(merge-pairs(xs))

fip fun merge-pairs(xs : partition<elem>) : <div> partition<elem>
  match(xs)
    Singleton(a, Singleton(b, xs1)) -> Singleton(merge(a, b, Unit2(Pad,Pad)), merge-pairs(xs1))
    Singleton(a, Sublist(b, xs1))   -> Singleton(merge-last-left(a, b, Unit2(Pad,Pad)), merge-pairs(xs1))
    Sublist(a, Singleton(b, xs1))   -> Singleton(merge-last-right(a, b, Unit2(Pad,Pad)), merge-pairs(xs1))
    Sublist(a, Sublist(b, xs1))     -> 
      if a > b then Singleton(STuple(b, a), merge-pairs(xs1))
               else Singleton(STuple(a, b), merge-pairs(xs1))
    xs -> xs

fip fun merge(c1 : sublist<elem>, c2 : sublist<elem>, u : unit2) : div sublist<elem>
  match c1
    SCons(a, cs1) -> match c2
      SCons(b, cs2) -> if a > b then SCons(b, merge(SCons(a, cs1), cs2, u))
                                else SCons(a, merge(cs1, SCons(b, cs2), u))
      STuple(b, c) -> merge-last2-left(SCons(a, cs1), b, c, u, Unit2(Pad,Pad))
    STuple(a, b) -> merge-last2-right(a, b, c2, u, Unit2(Pad,Pad))

fip fun merge-last2-right(a : elem, b : elem, c2 : sublist<elem>, u1 : unit2, u2 : unit2) : div sublist<elem>
  match c2
    SCons(c, cs2) -> 
      if a > c then SCons(c, merge-last2-right(a, b, cs2, u1, u2))
               else SCons(a, merge-last-right(b, SCons(c, cs2), u1))
    STuple(c, d) -> 
      if a > c then 
        if a > d   then SCons(c, SCons(d, STuple(a, b)))
        elif b > d then SCons(c, SCons(a, STuple(d, b)))
                   else SCons(c, SCons(a, STuple(b, d))) 
      elif b > c then 
        if b > d  then SCons(a, SCons(c, STuple(d, b)))
                  else SCons(a, SCons(c, STuple(b, d)))
      else SCons(a, SCons(b, STuple(c, d)))
     
fip fun merge-last-right(a : elem, c2 : sublist<elem>, u1 : unit2) : div sublist<elem>
  match c2
    SCons(c, cs2) -> 
      if a > c then SCons(c, merge-last-right(a, cs2, u1))
               else SCons(a, SCons(c, cs2))
    STuple(b, c) -> 
      if a > b then 
        if a > c then SCons(b, STuple(c, a))
                else SCons(b, STuple(a, c))
      else SCons(a, STuple(b, c))

fip fun merge-last2-left(c2 : sublist<elem>, a : elem, b : elem, u1 : unit2, u2 : unit2) : div sublist<elem>
  match c2
    SCons(c, cs2) -> 
      if a >= c then SCons(c, merge-last2-left(cs2, a, b, u1, u2))
                else SCons(a, merge-last-left(SCons(c, cs2), b, u1))
    STuple(c, d) -> 
      if a >= c then 
        if a >= d   then SCons(c, SCons(d, STuple(a, b)))
        elif b >= d then SCons(c, SCons(a, STuple(d, b)))
                    else SCons(c, SCons(a, STuple(b, d)))
      elif b >= c then 
        if b >= d then SCons(a, SCons(c, STuple(d, b)))
                  else SCons(a, SCons(c, STuple(b, d)))
      else SCons(a, SCons(b, STuple(c, d)))

fip fun merge-last-left(c2 : sublist<elem>, a : elem, u1 : unit2) : div sublist<elem>
  match c2
    SCons(c, cs2) -> 
      if a >= c then SCons(c, merge-last-left(cs2, a, u1))
                else SCons(a, SCons(c, cs2))
    STuple(b, c) -> 
      if a >= b then 
        if a >= c then SCons(b, STuple(c, a))
                  else SCons(b, STuple(a, c))
      else SCons(a, STuple(b, c))

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
