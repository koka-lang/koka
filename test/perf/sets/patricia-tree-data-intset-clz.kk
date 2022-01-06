// Patricia trees as implemented in Haskells Data.IntSet
// This is similar to patricia-tree-big-endian-okasaki
// with one modification: A Leaf represents 32 integers at a time
// by indicating in a bitmap (=int32) if an integer is contained.
// The key in each leaf will have its 5 least significant bits set to zero
// and we use the last 5 bits from a query to obtain a bitmap index.
// If the index is set, the query is in the tree and else it isn't.

module patricia-tree-data-intset-clz

import std/num/int32
import base

alias elem = int

type tree {
  Empty
  Leaf(key : int32, bm : int32)
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

val suffix-bit-mask = 31.int32
val prefix-bit-mask = not(suffix-bit-mask)

fun prefix-of(x) {
  and(x, prefix-bit-mask)
}

fun suffix-of(x) {
  and(x, suffix-bit-mask)
}

fun bitmap-of(x) {
  shl(1.int32, suffix-of(x))
}

fun lookup(k, ^t) {
  match(t) {
    Empty -> False
    Leaf(j, bm) -> prefix-of(k) == j && bitmap-of(k).and(bm) != 0.int32
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

pub extern clz32( x : int32 ) : int32 {
  c inline "(int32_t)(kk_bits_clz((uint32_t)(#1)))"
}

val min32 = min-int32

pub fun highest-bit(x0 : int32) {
  if(x0 == 0.int32) then 0.int32 else min32.shr(clz32(x0))
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
  val kx = prefix-of(k)
  val bm = bitmap-of(k)
  fun ins(t) {
    match(t) {
      Empty -> Leaf(kx, bm)
      Leaf(j, bm') -> if(j == kx) then Leaf(j, bm.or(bm'))
        else join(kx, Leaf(kx, bm), j, t)
      Branch(p, m, l, r) ->
        if(match-prefix(kx, p, m)) then if(zero-bit(kx, m))
            then Branch(p, m, ins(l), r)
            else Branch(p, m, l, ins(r))
          else join(kx, Leaf(kx, bm), p, t)
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

pub fun count-patricia-tree-data-intset-clz(start : int, end : int, xs : list<int>) {
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
