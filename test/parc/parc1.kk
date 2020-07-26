fun foo0(x) {
  x
}

fun foo1(x, y) {
  // drop y
  return x
}

fun foo2(x:int, y:int) {
  // drop y, dup x
  return x + x
}

fun foo3(x:list<a>): list<a> {
  // dup x
  return x + x
}

fun foo4(x:list<int>): list<int> {
  // drop x
  return []
}

fun foo5(x:list<int>): list<int> {
  val y = match(x) {
    Cons -> x
    Nil -> /* drop x */ []
  }
  return y
}

fun foo6(x:int):int {
  val y = match(x + 1) { /* store in x' */
    2 -> /* drop x, x' */ 3
    _ -> /* drop x' */ x
  }
  return y
}

fun foo7(x:int): (() -> int) {
  // no dups/drops
  return fun() { x }
}

fun match1(xs:list<int>, y:int) {    // drop y once
  match(xs) {
    Cons(x,_) -> match(xs) {
                   Cons -> x
                   Nil -> 2
                 }
    Cons -> 2
    _ -> y
  }
}

fun match2(xs:list<int>, y:int, z:int) {    // drop y once, drop z early
  match(xs) {
    Cons(x,_) -> match(xs) {
                   Cons -> x
                   Nil -> 2
                 }
    Cons -> 2
    _ -> y
  }
}


fun match3(xs:list<int>) {
  match(xs) {
    Cons as ys -> ys  // don't dup(ys); drop(xs)
    Nil -> []
  }
}

fun match4(xs:list<a>, ys:list<a>): list<a> {
  match(xs) {
    Cons(z) -> match(ys) {
                 Cons(z) -> [z]
                 Nil -> xs
               }
    Nil -> ys
  }
}

fun match5(xs:list<int>) {
  match(xs) {
    // this 'ys' cannot be changed to 'xs' to be even more ambiguous
    Cons(xs) as ys -> xs
    Nil -> 1
  }
}

fun match6(xs:list<int>) {
  match(xs) {
    Cons as ys | ys.length == 2 -> ys  // don't dup(ys); drop(xs)
    Nil -> []
  }
}
