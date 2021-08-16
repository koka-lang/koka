// Skip Lists: A Probabilistic Alternative to Balanced Trees, Pugh
// We fix the dice, p = 0.5.

module skip-list-imperative

import std/num/random

alias elem = int

struct skiplist<h> {
  level : int
  forward : vector<ref<h,skiplist_<h>>>
}

fun empty() {
  Skiplist(1, vector(1, ref(End)))
}

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

type skiplist_<h> {
  Node(key : elem, forward : vector<ref<h,skiplist_<h>>>)
  End
}

public fun shows(s : skiplist<h>) : <read<h>, div> string {
    show(s.level) ++ ", " ++ show_(s.forward)
}

fun shows_(s : skiplist_<h>) : <read<h>, div> string {
    match(s) {
        End -> "x"
        Node(k, f) -> "Node(" ++ show(k) ++ ", " ++ f.show_ ++ ")"
    }
}

fun show_(v : vector<ref<h, skiplist_<h>>>) : <read<h>, div> string {
    v.map(fn(r) { shows_(!r) }).list().join(", ")
}

fun search(list : skiplist<h>, searchKey : elem) : <read<h>, exn, div> bool {
  fun go(forward : vector<ref<h, skiplist_<h>>>, i : int) : <read<h>, exn, div> bool {
    if(i >= 0) {
      match(!forward[i]) {
        Node(k, f) | k < searchKey -> go(f, i)
        _ -> go(forward, i - 1)
      }
    } else match(!forward[0]) {
        Node(k, _) -> k == searchKey
        _ -> False
    }
  }
  go(list.forward, list.level - 1)
}

fun insert(l : skiplist<h>, searchKey : elem) : <st<h>, exn, div, random> skiplist<h> {
  val lvl = random-level(l.level)
  fun go(forward : vector<ref<h, skiplist_<h>>>, i : int, update : vector<ref<h, skiplist_<h>>>) : <st<h>, exn, div> () {
    if(i >= 0) {
      match(!forward[i]) {
        Node(k, f) | k < searchKey -> {
            go(f, i, update)
        }
        _ {
          var u := update
          u[i] := forward[i]
          go(forward, i - 1, u)
        }
      }
    } else match(!forward[0]) {
        Node(k, _) | k == searchKey -> ()
        _ {
          val f = list(1, lvl, fn(i) { ref(End) }).vector
          val x = Node(searchKey, f)
          for(0, lvl - 1, fn(i) {
            f[i].set(!update[i])
            update[i].set(x)
          })
        }
    }
  }
  val forward = if(lvl > l.level) {
    val new = ref(End) // 'new' will be used in the new vector only once!
    vector-init(lvl, fn(i) { l.forward.at(i).maybe(new) })
  } else l.forward
  val level = max(l.level, lvl)
  val never-used = ref(End)
  val update = vector-init(lvl, fn(i) { forward.at(i).maybe(never-used) })
  go(forward, level - 1, update)
  Skiplist(level, forward)
}

// Benchmark

public fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(t, x))
    }
  }
  pseudo-random(503, fn() { go(xs, empty()) })
}

public fun count-skip-list-imperative(start : int, end : int, xs : list<int>) {
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