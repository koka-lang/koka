// Weight balanced binary trees as in Haskells Data.Set
// This is similar to the presentation by Hirai and Yamamoto.
// Unfortunately, there are some performance tricks from Haskell
// that we can't use here:
//  - Haskell avoids balancing a tree if it is pointer equal to the
//    tree before the insert. But we can't detect if two trees are
//    pointer equal and keeping a 'changed' flag in the tree itself
//    is too expensive.
//  - Haskell can avoid inserting an element when it is both equal
//    and pointer equal to an element in the tree. We simply insert
//    the element, since this tree only used for integers.

module weight-balanced-data-set

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
       -> match(compare(kx, ky)) {
         Lt -> balanceL(ky, insert(kx, l), r)
         Gt -> balanceR(ky, l, insert(kx, r))
         Eq -> Bin(sz, kx, l, r)
       }
  }
}

fun member(kx, t) {
  match(t) {
    Tip -> False
    Bin(_, ky, l, r)
      -> match(compare(kx, ky)) {
        Lt -> member(kx, l)
        Gt -> member(kx, r)
        Eq -> True
      }
  }
}

fun balanceR(x, l, r) {
  match(l) {
    Tip -> match(r) {
      Tip -> Bin(1.int32, x, Tip, Tip)
      Bin(rs, rx, rl, rr) -> match (rl, rr) {
        (Tip, Tip) -> Bin(2.int32, x, Tip, r)
        (Tip, Bin(_, _, _, _)) -> Bin(3.int32, rx, Bin(1.int32, x, Tip, Tip), rr)
        (Bin(_, rlx, _, _), Tip) -> Bin(3.int32, rlx, Bin(1.int32, x, Tip, Tip), Bin(1.int32, rx, Tip, Tip))
        (Bin(rls, rlx, rll, rlr), Bin(rrs, _, _, _))
          | rls < gamma*rrs -> Bin(1.int32 + rs, rx, Bin(1.int32 + rls, x, Tip, rl), rr)
          | True -> Bin(1.int32 + rs, rlx, Bin(1.int32 + size(rll), x, Tip, rll), Bin(1.int32 + rrs + size(rlr), rx, rlr, rr))
      }
    }
    Bin(ls, _, _, _)
      -> match(r) {
        Tip -> Bin(1.int32 +ls, x, l, Tip)
        Bin(rs, rx, rl, rr)
          | rs > delta * ls -> match (rl, rr) {
            (Bin(rls, rlx, rll, rlr), Bin(rrs, _, _, _))
              | rls < gamma*rrs -> Bin(1.int32 +ls+rs, rx, Bin(1.int32 +ls+rls, x, l, rl), rr)
              | True -> Bin(1.int32 +ls+rs, rlx, Bin(1.int32 +ls+size(rll), x, l, rll), Bin(1.int32 +rrs+size(rlr), rx, rlr, rr))
            _ -> wont-happen()
          }
          | True -> Bin(1.int32 +ls+rs, x, l, r)
      }
  }
}

fun balanceL(x, l, r) {
  match(r) {
    Tip -> match(l) {
      Tip -> Bin(1.int32, x, Tip, Tip)
      Bin(ls, lx, ll, lr) -> match (ll, lr) {
        (Tip, Tip) -> Bin(2.int32, x, l, Tip)
        (Tip, Bin(_, lrx, _, _)) -> Bin(3.int32, lrx, Bin(1.int32, lx, Tip, Tip), Bin(1.int32, x, Tip, Tip))
        (Bin(_, _, _, _), Tip) -> Bin(3.int32, lx, ll, Bin(1.int32, x, Tip, Tip))
        (Bin(lls, _, _, _), Bin(lrs, lrx, lrl, lrr))
          | lrs < gamma*lls -> Bin(1.int32 + ls, lx, ll, Bin(1.int32 + lrs, x, lr, Tip))
          | True -> Bin(1.int32 + ls, lrx, Bin(1.int32 + lls + size(lrl), lx, ll, lrl), Bin(1.int32 + size(lrr), x, lrr, Tip))
      }
    }
    Bin(rs, _, _, _) -> match(l) {
      Tip -> Bin(1.int32 + rs, x, Tip, r)
      Bin(ls, lx, ll, lr)
        | ls > delta*rs -> match(ll, lr) {
          (Bin(lls, _, _, _), Bin(lrs, lrx, lrl, lrr))
            | lrs < gamma*lls -> Bin(1.int32 + ls + rs, lx, ll, Bin(1.int32 + rs + lrs, x, lr, r))
            | True -> Bin(1.int32 + ls + rs, lrx, Bin(1.int32 + lls + size(lrl), lx, ll, lrl), Bin(1.int32 + rs + size(lrr), x, lrr, r))
          _ -> wont-happen()
        }
        | True -> Bin(1.int32 + ls + rs, x, l, r)
    }
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

public fun count-weight-balanced-data-set(start : int, end : int, xs : list<int>) {
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