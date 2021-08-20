// Balancing weight-balanced trees, Hirai and Yamamoto
// We use the variant balancing method with (3,2) to match Haskells Data.Set

module weight-balanced-hirai

import std/num/int32
import base

alias elem = int

val delta = 3.int32
val gamma = 2.int32

type tree {
  Tip
  Bin(s : int32, a : elem, l : tree, r : tree)
}

fun size(t) {
  match(t) {
    Tip -> 0.int32
    Bin(sz, _, _, _) -> sz
  }
}

fun singleton(k) {
  Bin(1.int32, k, Tip, Tip)
}

fun insert(kx, t) {
  match(t) {
    Tip -> singleton(kx)
    Bin(sz, ky, l, r)
       -> if(kx <= ky) then
            if(kx == ky) then Bin(sz, ky, l, r)
            else balanceR(ky, insert(kx, l), r)
          else balanceL(ky, l, insert(kx, r))
  }
}

fun member(kx, t) {
  match(t) {
    Tip -> False
    Bin(_, ky, l, r)
      -> if(kx <= ky) then
           if(kx == ky) then True
           else member(kx, l)
         else member(kx, r)
  }
}

fun is-balanced(a, b) {
  val x = size(a)
  val y = size(b)
  x + y <= 1.int32 || delta * x >= y
}

fun is-single(a, b) {
  size(a) < gamma * size(b)
}

// By induction the size information in l and r is correct.
inline fun balanceL(k, l, r) {
  if(is-balanced(l, r)) then Bin(size(l) + size(r) + 1.int32, k, l, r)
      else rotateL(k, l, r)
}

fun rotateL(k, l, r) {
  match(r) {
    Bin(_, _, rl, rr) -> if(is-single(rl, rr)) then singleL(k, l, r) else doubleL(k, l, r)
    _ -> wont-happen()
  }
}

fun singleL(k, l, r) {
  match(r) {
    Bin(sz, k2, l2, r2) -> Bin(sz + size(l) + 1.int32, k2, Bin(size(l) + size(l2) + 1.int32, k, l, l2), r2)
    _ -> wont-happen()
  }
}

fun doubleL(k, l, r) {
  match(r) {
    Bin(s2, k2, Bin(_, k3, l3, r3), r2) -> Bin(s2 + size(l) + 1.int32, k3, Bin(size(l) + size(l3) + 1.int32, k, l, l3), Bin(size(r3) + size(r2) + 1.int32, k2, r3, r2))
    _ -> wont-happen()
  }
}

inline fun balanceR(k, l, r) {
  if(is-balanced(r, l)) then Bin(size(l) + size(r) + 1.int32, k, l, r)
      else rotateR(k, l, r)
}

fun rotateR(k, l, r) {
  match(l) {
    Bin(_, _, ll, lr) -> if(is-single(lr, ll)) then singleR(k, l, r) else doubleR(k, l, r)
    _ -> wont-happen()
  }
}

fun singleR(k, l, r) {
  match(l) {
    Bin(s2, k2, l2, r2) -> Bin(s2 + size(r) + 1.int32, k2, l2, Bin(size(r2) + size(r) + 1.int32, k, r2, r))
    _ -> wont-happen()
  }
}

// (l2 < k2 (l3 < k3 < r3)) < k < r
fun doubleR(k, l, r) {
  match(l) {
    Bin(s2, k2, l2, Bin(_, k3, l3, r3)) -> Bin(s2 + size(r) + 1.int32, k3, Bin(size(l2) + size(l3) + 1.int32, k2, l2, l3), Bin(size(r3) + size(r) + 1.int32, k, r3, r))
    _ -> wont-happen()
  }
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

public fun count-weight-balanced-hirai(start : int, end : int, xs : list<int>) {
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