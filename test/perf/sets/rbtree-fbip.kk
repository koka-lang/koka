// The zipper-transform of splay-trees-fbip applied to rbtree-okasaki

module rbtree-fbip

alias elem = int

type color {
  R
  B
}

type rbtree {
  E
  T(c : color, l : rbtree, e : elem, r : rbtree)
}

type rbzipper {
  Top
  GR(c : color, l : rbtree, e : elem, p : rbzipper)
  GL(c : color, p : rbzipper, e : elem, r : rbtree)
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

fun balance(c, l, e, r, p) {
  match(c, p) {
    (R, GR(R, l1, e1, GR(c2, l2, e2, p2))) -> balance(R, T(B, l2, e2, l1), e1, T(B, l, e, r), p2)
    (R, GR(R, l1, e1, GL(c2, p2, e2, r2))) -> balance(R, T(B, l1, e1, l), e, T(B, r, e2, r2), p2)
    (R, GL(R, GR(c2, l2, e2, p2), e1, r1)) -> balance(R, T(B, l2, e2, l), e, T(B, r, e1, r1), p2)
    (R, GL(R, GL(c2, p2, e2, r2), e1, r1)) -> balance(R, T(B, l, e, r), e1, T(B, r1, e2, r2), p2)
    (_, GR(c1, l1, e1, p1)) -> balance(c1, l1, e1, T(c, l, e, r), p1)
    (_, GL(c1, p1, e1, r1)) -> balance(c1, T(c, l, e, r), e1, r1, p1)
    (_, Top) -> T(c, l, e, r)
  } 
}

fun balanceB(s, zipper) {
  match(zipper) {
    Top -> s
    GR(c, l, e, p) -> balance(c, l, e, s, p)
    GL(c, p, e, r) -> balance(c, s, e, r, p)
  }
}

fun insert(x, s) {
  fun ins(s, z) {
    match(s) {
      E -> balance(R, E, x, E, z)
      T(R, a, y, b) {
        if(x < y) then ins(a, GL(R, z, y, b))
        elif(y < x) then ins(b, GR(R, a, y, z))
        else balance(R, a, y, b, z)
      }
      T(B, a, y, b) {
        if(x < y) then ins(a, GL(B, z, y, b))
        elif(y < x) then ins(b, GR(B, a, y, z))
        else balanceB(s, z)
      }
    }
  }
  val T(_, a, y, b) = ins(s, Top)
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

pub fun count-rbtree-fbip(start : int, end : int, xs : list<int>) {
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