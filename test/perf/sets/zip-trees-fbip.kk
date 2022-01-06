// Zip Trees, Tarjan, Levy and Timmel

module zip-trees-fbip

import std/num/random

alias elem = int;

struct set {
  randState : randState
  tree : tree
}

type tree {
  Bin(rank : int32, key : elem, left : tree, right : tree)
  Tip
}

fun empty() {
  Set(RandState(2020832078.int32, 196622394.int32,
    4171630633.int32, 291524055.int32), End)
}

fun search(l, k) {
  fun go(p) {
    match(p) {
      Tip -> False
      Bin(_, k2, l, r) ->
        if(k2 < k) then go(l)
        elif(k2 == k) then True
        else go(r)
    }
  }
  go(l.tree)
}

fun insert(l, k) {
  fun unzip(tree) {
    match(tree) {
      Bin(rk, y, l, r) {
        if(k < y) {
          val (a, b) = unzip(l)
          (a, Bin(rk, y, b, r)) 
        }
      }
      Tip -> (Tip, Tip)
    }
  }

  fun go(rank, tree) {
    match(tree) {
      Tip -> Bin(rank, k, Tip, Tip)
      Bin(rk, k2, l, r) {
        if(rk < rank || (k2 >= k & rk == rank)) {
          // unzip and add new node
        }
        if(k2 < k) {
           
        }
      }
    }
  }
  match(l) {
    Set(randState, tree) {
      val (rand, randState') = next randState
      Set(randState', go(geo(rand), tree))
    }
  }
}

fun geo(x0) {
  lowest-bit(x0)
}

inline fun lowest-bit(x0) {
  x0.and(negate(x0))
}

// xoshiro128++ 1.0 by Blackman and Vigna, pub domain (CC0)
// Adapted from https://prng.di.unimi.it/xoshiro128plusplus.c

struct randState {
  s0 : int32
  s1 : int32
  s2 : int32
  s3 : int32
}

fun rotl(x, k) {
  x.shl(k).or(x.shr(32 - k))
}

fun next(st) {
  val result = rotl(st.s0 + st.s3, 7) + st.s0
  val t = st.s1.shl(9)
  val s2' = st.s2.xor(st.s0)
  val s3' = st.s3.xor(st.s1)
  val s1' = st.s1.xor(s2')
  val s0' = st.s0.xor(s3')
  val s2'' = s2'.xor(t)
  val s3'' = rotl(s3', 11)
  (result, RandState(s0', s1', s2'', s3''))
}

// Benchmark

pub fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(t, x))
    }
  }
  pseudo-random(503, fn() { go(xs, empty()) })
}

pub fun count-zip-trees-fbip(start : int, end : int, xs : list<int>) {
  val t = from-list(xs)
  fun go(i, acc) {
    if(i <= end) {
      match(search(t, i)) {
        True -> go(i+1, acc+1)
        False -> go(i+1, acc)
      }
    } else acc
  }
  go(start, 0)
}