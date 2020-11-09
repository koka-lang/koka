/* Basic syntax examples */
module syntax/basic

// Named function declarations use `fun`
// while anonymous function expressions use `fn`
public fun test1() {
  map( list(1,5), fn(i){ println(i) } )
}

// Use the dot-syntax to chain functions calls:
// `e.f(e1,...,eN)  ~>  f(e,e1,...,eN)`
// (in Koka since 2012)
public fun test2() {
  list(1,5).map( fn(i){ println(i) } )
}

// `fn` function arguments can be put outside the parenthesis.
// This is called `trailing lambdas` (and Koka was the first language to have this)
public fun test3() {
  list(1,5).map fn(i){
    println(i)
  }
}

// An anonymous function without arguments can be written
// just with a pair of curly braces:
// `{ ... }  ===  fn(){ ... }`
// This is very useful to define "control-flow" functions

fun twice(f) {
  f()
  f()
}

public fun test4() {
  twice { println("hi") }
  // == twice( fn(){ println("hi" ) } )
}

// This is very useful to have control operation folding
// like `while` as regular functions:
public fun test5() {
  var i := 0
  while{ i < 10 } {
    i := i + 1
    println(i)
  }
}

// In Koka, everything between curly braces ("suspenders")
// is not directly evaluated but may be never evaluated, or
// evaluated multiple times (as in `while`).
// Things between parenthesis are evaluated to a value
// (including their effects) before a call.
//
// This is why there is no ML-style currying (like `f e1 e2`)
// as we need  to distinguish `f(e1)(e2)` versus `f(e1,e2)`: in the
// first form there may be a side effect induced by `f` before
// `e2` is evaluated and passed to the result of `f(e1)`
// while in the second form both `e1` and `e2` are evaluated
// before calling `f`.
public fun main() {
  [21,21].foldl(0,(+)).println
}
