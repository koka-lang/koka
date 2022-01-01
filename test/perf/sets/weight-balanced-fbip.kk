// Balancing weight-balanced trees, Hirai and Yamamoto
// We use the variant balancing method with (3,2) to match Haskells Data.Set
// We apply the zipper transform trick of splay-trees-fbip here

module weight-balanced-fbip

import std/num/int32
import base

alias elem = int

val delta = 3.int32
val gamma = 2.int32

type tree {
  Tip
  Bin(s : int32, a : elem, l : tree, r : tree)
}

type zipper {
  Root
  GL(s : int32, a : elem, p : zipper, r : tree)
  GR(s : int32, a : elem, r : tree, p : zipper)
}

type container {
  Con(s : int32, a : elem, l : tree, r : tree)
}

fun size(^t) {
  match(t) {
    Tip -> 0.int32
    Bin(sz, _, _, _) -> sz
  }
}

fun insert(kx, t) : tree {
  fun ins(t, z) {
    match(t) {
      Tip -> balance_(Bin(1.int32, kx, Tip, Tip), z)
      Bin(sz, ky, l, r)
        -> if(kx <= ky) then
              if(kx == ky) then balance_(Bin(sz, ky, l, r), z)
              else ins(l, GL(sz, ky, z, r))
            else ins(r, GR(sz, ky, l, z))
    }
  }
  ins(t, Root)
}

fun member(kx, ^t) : bool {
  match(t) {
    Tip -> False
    Bin(_, ky, l, r)
      -> if(kx <= ky) then
           if(kx == ky) then True
           else member(kx, l)
         else member(kx, r)
  }
}

fun balance_(t, z) : tree {
  match(z) {
    Root -> t
    GR(_, k, l, p) -> balance(Con(size(l) + size(t) + 1.int32, k, l, t), p)
    GL(_, k, p, r) -> balance(Con(size(r) + size(t) + 1.int32, k, t, r), p)
  }
}

// By induction 's' is correct.
fun balance(c, p) : tree {
  match(c) {
    Con(s, k, l, r) -> match(p) {
      Root -> Bin(s, k, l, r)
      GR(s1, k1, l1, p1)
        | is-balanced-size(size(l1), s) -> balance(Con(size(l1) + s + 1.int32, k1, l1, Bin(s, k, l, r)), p1)
        | True -> if(is-single(l, r)) then {
            val sz = size(l1) + size(l) + 1.int32
            balance(Con(size(r) + sz + 1.int32, k, Bin(sz, k1, l1, l), r), p1)
          } else match(l) {
            Bin(s0, k0, l0, r0) {
              val sz1 = size(l1) + size(l0) + 1.int32
              val sz2 = size(r0) + size(r) + 1.int32
              balance(Con(sz1 + sz2 + 1.int32, k0, Bin(sz1, k1, l1, l0), Bin(sz2, k, r0, r)), p1)
            }
            _ -> wont-happen()
          }
      GL(s1, k1, p1, r1)
        | is-balanced-size(size(r1), s) -> balance(Con(size(r1) + s + 1.int32, k1, Bin(s, k, l, r), r1), p1)
        | True -> if(is-single(r, l)) then balance(Con(size(r1) + s + 1.int32, k, l, Bin(size(r1) + size(r) + 1.int32, k1, r, r1)), p1)
          else match(r) {
            Bin(s0, k0, l0, r0) {
              val sz1 = size(l) + size(l0) + 1.int32
              val sz2 = size(r0) + size(r1) + 1.int32
              balance(Con(sz1 + sz2 + 1.int32, k0, Bin(sz1, k, l, l0), Bin(sz2, k1, r0, r1)), p1)
            }
            _ -> wont-happen()
          }
    }
  }
}

fun is-balanced-size(x : int32, y : int32) {
  x + y <= 1.int32 || delta * x >= y
}

fun is-single(^a, ^b) {
  size(a) < gamma * size(b)
}

// Benchmark

fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(x, t))
    }
  }
  go(xs, Tip)
}

pub fun count-weight-balanced-fbip(start : int, end : int, xs : list<int>) {
  val t = from-list(xs)
  fun go(i, acc) {
    if(i <= end) {
      match(member(i, t)) {
        True -> go(i+1, acc+1)
        False -> go(i+1, acc)
      }
    } else acc
  }
  go(start, 0)
}