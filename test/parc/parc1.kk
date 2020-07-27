fun foo0(x) {
  x
}

fun foo1(x, y) {
  // drop y
  return x
}

fun foo2(x:int, y:int) {
  // drop y
  return /* dup x */ x + x
}

fun foo3(x:list<a>): list<a> {
  return /* dup x */ x + x
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
  // dup x
  val y = match(x + 1 /* x' */) {
    2 -> /* drop x, x' */ 3
    _ -> /* drop x' */ x
  }
  return y
}

fun foo7(x:int): (() -> int) {
  return fun() { x }
}

fun foo8(x:int, y:a, z:a): a {
  match(x) {
    0 -> /* drop x, z */ y
    _ -> /* drop x, y */ z
  }
}

fun match1(xs:list<int>, y:int) {
  match(xs) {
    Cons(x,_) ->
      /* dup x; drop y */
      match(xs) {
        Cons -> /* drop xs */ x
        Nil -> /* drop x, xs */ 2
      }
    Cons -> /* drop y, xs */ 2
    _ -> /* drop xs */ y
  }
}

fun match2(xs:list<int>, y:int, z:int) {
  // drop z
  match(xs) {
    Cons(x,_) ->
      // dup x; drop y
      match(xs) {
        Cons -> /* drop xs */ x
        Nil -> /* drop x, xs */ 2
      }
    Cons -> /* drop y, xs */ 2
    _ -> /* drop xs */ y
  }
}


fun match3(xs:list<int>) {
  match(xs) {
    Cons as ys -> ys
    Nil -> /* drop xs */ []
  }
}

fun match4(xs:list<a>, ys:list<a>): list<a> {
  match(xs) {
    Cons(z) -> match(ys) {
                 Cons(z /* z' */) -> /* dup z'; drop xs; drop ys */ [z /* z' */]
                 Nil -> /* drop ys */ xs
               }
    Nil -> /* drop xs */ ys
  }
}

fun match5(xs:list<int>) {
  match(xs) {
    Cons(xs /* xs' */) as ys -> /* dup xs'; drop xs */ xs /* xs' */
    Nil -> /* drop xs */ 1
  }
}

fun match6(xs:list<int>) {
  match(xs) {
    Cons(x) -> /* dup(x) */ Cons(x /* box(x) */, xs)
    Nil -> /* drop xs */ Nil
  }
}
