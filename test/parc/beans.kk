// Examples from "Counting Immutable Beans: Reference Counting
//                Optimized for Purely Functional Programming"
//  by Sebastian Ullrich, Karlsruhe Institute of Technology
//     Leonardo de Moura, Microsoft Research
// https://arxiv.org/pdf/1908.05647.pdf

type pair<a> { Pair(x: a, y: a) }

fun id(x) { x }

fun mkPairOf(x) { Pair(x, x) }

fun fst(x, y) { x }

fun isNil(xs) {
  match(xs) {
    Nil -> True
    Cons -> False
  }
}

fun hasNothing(xs) {
  match(xs) {
    Nil -> False
    Cons(Nothing) -> True
    Cons(_, xs') -> hasNothing(xs')
  }
}

fun map(f: a -> a, xs: list<a>): list<a> {
  match(xs) {
    Nil -> xs
    Cons(x, s) -> Cons(f(x), map(f, s))
  }
}

fun goForward(p) {
  // todo: why isn't this match considered exhaustive?
  // todo: why doesn't it reuse Pair, but it does reuse Cons?
  match(p) {
    Pair(Nil, bs) -> Pair(Nil, bs)
    Pair(Cons(x, xs), bs) -> Pair(xs, Cons(x, bs))
  }
}

fun swap(lst) {
  // todo: why isn't this match considered exhaustive?
  match(lst) {
    Nil -> Nil
    Cons(x, Nil) -> Cons(x, Nil)
    Cons(x, Cons(y, zs)) -> Cons(y, Cons(x, zs))
  }
}

fun borrow-tail(f: (a, a) -> a, y: a): a {
  f(y, y)
}

/*
todo: what is this function?

f x = case x of
  (let r = proj1 ; ret r)
  (let y1 = ctor1 ; let y2 = ctor1 y1 ; let r = f y2 ; ret r)

from section "Preserving tail calls", page 7
*/
