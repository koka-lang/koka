// Fast mergeable integer maps, Okasaki and Gill
// We find the highest bit through some ors and right-shifts
// and not a loop (just like Data.IntSet in Haskell).

module patricia-tree-big-endian-okasaki

import std/num/int32
import base

alias elem = int

type tree {
  Empty
  Leaf(key : int32)
  Branch(pre : int32, bit : int32, l : tree, r : tree)
}

fun zero-bit(k, m) {
  and(k, m) == 0.int32
}

fun mask-bits(k, m) {
  and(or(k, m - 1.int32), not(m))
}

fun match-prefix(k, p, m) {
  mask-bits(k, m) == p
}

fun lookup(k, t) {
  match(t) {
    Empty -> False
    Leaf(j) -> j == k
    Branch(p, m, l, r) ->
      if(match-prefix(k, p, m)) {
        if(zero-bit(k, m))
          then lookup(k, l)
          else lookup(k, r)
      } else False
  }
}

fun br(p, m, l, r) {
  match(l, r) {
    (Empty, _) -> r
    (_, Empty) -> l
    (_, _) -> Branch(p, m, l, r)
  }
}

fun highest-bit(x0 : int32) {
  val x1 = x0.or(shr(x0,  1.int32))
  val x2 = x1.or(shr(x1,  2.int32))
  val x3 = x2.or(shr(x2,  4.int32))
  val x4 = x3.or(shr(x3,  8.int32))
  val x5 = x4.or(shr(x4, 16.int32))
  x5^shr(x5, 1.int32)
}

fun branching-bit(p0, p1) {
  highest-bit(p0^p1)
}

fun join(p0, t0, p1, t1) {
  val m = branching-bit(p0, p1)
  if(zero-bit(p0, m))
    then Branch(mask-bits(p0, m), m, t0, t1)
    else Branch(mask-bits(p0, m), m, t1, t0)
}

fun insert(k, t) {
  fun ins(t) {
    match(t) {
      Empty -> Leaf(k)
      Leaf(j) -> if(j == k) then Leaf(j)
        else join(k, Leaf(k), j, t)
      Branch(p, m, l, r) ->
        if(match-prefix(k, p, m)) then if(zero-bit(k, m))
            then Branch(p, m, ins(l), r)
            else Branch(p, m, l, ins(r))
          else join(k, Leaf(k), p, t)
    }
  }
  ins(t)
}

// Benchmark
fun from-list(xs : list<int>) {
  fun go(xs : list<int>, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(x.int32, t))
    }
  }
  go(xs, Empty)
}

public fun count-patricia-tree-big-endian-okasaki(start : int, end : int, xs : list<int>) {
  val t = from-list(xs)
  fun go(i, acc) {
    if(i <= end) {
      match(lookup(i.int32, t)) {
        True -> go(i+1, acc+1)
        False -> go(i+1, acc)
      }
    } else acc
  }
  go(start, 0)
}