// Section 3.3

module rbtree-okasaki

alias elem = int

type color {
  R
  B
}

type rbtree {
  E
  T(c : color, l : rbtree, e : elem, r : rbtree)
}

fun member(x, ^t) {
  fun mem(z : elem, ^t) {
    match(t) {
      E -> x == z
      T(_, a, y, b) -> if(x <= y) then mem(y, a) else mem(z, b)
    }
  }
  match(t) {
    E -> False
    T(_, a, y, b) {
      if(x <= y) then mem(y, a)
      else member(x, b)
    }
  }
}

fun lbalance(c1, a1, z, d) {
  match(a1) {
    T(_, T(R, a, x, b), y, c) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    T(_, a, x, T(R, b, y, c)) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    _ -> T(c1, a1, z, d)
  }
}

fun rbalance(c1, a, x, b1) {
  match(b1) {
    T(_, T(R, b, y, c), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    T(_, b, y, T(R, c, z, d)) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    _ -> T(c1, a, x, b1)
  }
}

fun is-red(t) {
  match(t) {
    T(R, _, _, _) -> True
    _ -> False
  }
}

fun insert(x, s) {
  fun ins(s) {
    match(s) {
      E -> T(R, E, x, E)
      T(R, a, y, b) {
        if(x < y) then T(R, ins(a), y, b)
        elif(y < x) then T(R, a, y, ins(b))
        else s
      }
      T(B, a, y, b) {
        if(x < y) then {
          if(is-red(a)) then lbalance(B, ins(a), y, b)
          else T(B, ins(a), y, b)
        } elif(y < x) then {
          if(is-red(b)) then rbalance(B, a, y, ins(b))
          else T(B, a, y, ins(b))
        } else s
      }
    }
  }
  val T(_, a, y, b) = ins(s)
  T(B, a, y, b)
}

// Benchmark

fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(x,t))
    }
  }
  go(xs, E)
}

public fun count-rbtree-okasaki(start : int, end : int, xs : list<int>) {
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