// Skip Lists: A Probabilistic Alternative to Balanced Trees, Pugh
// We fix the dice, p = 0.5.
// We don't store the pointers from smaller-level elements to
// larger-level elements since these are not essential to the datastructure.
// This is necessary for a functional implementation, since otherwise
// we would have to update most of the list when we update one cell.

module skip-list-vector

import std/num/random
import base

alias elem = int

struct skiplist {
  level : int
  head : skiplist_
}

val empty = Skiplist(0, End)

fun random-level(level) {
  fun go(n) {
    if(random-bool() || n >= level + 1) {
      n
    } else {
      go(n + 1)
    }
  }
  go(1)
}

type skiplist_ {
  Node(key : elem, forward : vector<skiplist_>)
  End
}

fun search(list : skiplist, searchKey) {
  fun go(key, forward : vector<skiplist_>, i) {
    if(i >= 0) {
      match(forward[i]) {
        Node(k, f) | k <= searchKey -> go(k, f, i)
        _ -> go(key, forward, i - 1)
      }
    } else {
      key == searchKey
    }
  }
  match(list.head) {
    Node(k, f) -> go(k, f, list.level - 1)
    End -> False
  }
}

// i < lvl - 1 = forward.length() - 1
fun re-assign(l, i : int, searchKey, forward) : <div, exn> (vector<skiplist_>, skiplist_) {
  fun go(l, i, forward) : <div, exn> (vector<skiplist_>, skiplist_) {
    if(i < 0) then (forward, l)
    else match(l) {
      Node(k, f) {
        match(f[i]) {
          fi as Node(k2, _) {
            if(k2 <= searchKey) {
              val (forward2, fi2) = go(fi, i, forward)
              var f2 := f
              f2[i] := fi2
              (forward2, Node(k, f2))
            } else {
              var f2 := f
              f2[i] := End
              var forward2 := forward
              forward2[i] := fi
              go(Node(k, f2), i - 1, forward2)
            }
          }
          End -> go(l, i - 1, forward)
        }
      }
      End -> (forward, l)
    }
  }
  go(l, i, forward)
}

// Invariants: lvl >= 1, l == Node(_,_), -1 <= i <= l.forward.length() - 1
fun place(l, i : int, searchKey : elem, lvl : int) : <div, exn> skiplist_ {
  if(i < lvl - 1) {
    val (forward, l2) = re-assign(l, i + 1, searchKey, vector(lvl, End))
    val x = Node(searchKey, forward)
    match(l2) {
      Node(k2, f) {
        var f2 := f
        f2[i + 1] := x
        Node(k2, f2)
      }
      End -> wont-happen()
    }
  } else match(l) {
    Node(k, f) | k == searchKey -> l
    Node(k, f) {
      match(f[i]) {
        fi as Node(k2, _) | k2 <= searchKey -> {
          val fi2 = place(fi, i, searchKey, lvl)
          var f2 := f
          f2[i] := fi2
          Node(k, f2)
        }
        _ -> place(l, i - 1, searchKey, lvl)
      }
    }
    End -> wont-happen()
  }
}

fun insert(list : skiplist, searchKey : elem) : <div, exn, random> skiplist {
  val lvl = random-level(list.level)
  match(list.head) {
    End -> {
      Skiplist(lvl, Node(searchKey, vector(lvl, End)))
    }
    Node(k, f) {
      val k2 = if(k >= searchKey) then searchKey else k
      val searchKey2 = if(k >= searchKey) then k else searchKey
      val f2 = if(lvl > list.level) {
        vector-init(lvl, fn(i) { f.at(i).maybe(End) })
      } else f
      val hd = Node(k2, f2)
      val level = max(list.level, lvl)
      Skiplist(level, place(hd, level - 1, searchKey2, lvl))
    }
  }
}

// Benchmark

fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(t, x))
    }
  }
  pseudo-random(503, fn() { go(xs, empty) })
}

public fun count-skip-list-vector(start : int, end : int, xs : list<int>) {
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