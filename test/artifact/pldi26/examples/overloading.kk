// overloading.kk — Static overloading examples
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 3: Static Overloading as Qualification (the [var-qualify] rule)
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/overloading.kk

module overloading

import std/num/float64


// ─────────────────────────────────────────────────────────────────────────────
// [var-qualify] rule: a plain name is elaborated to a qualified name
// when the qualified name uniquely matches the required type.
// ─────────────────────────────────────────────────────────────────────────────

fun int/showx( x : int ) : string
  x.show

fun float64/showx( f : float64 ) : string
  f.show

fun char/showx( c : char ) : string
  "'" ++ [c].string ++ "'"

fun bool/showx( b : bool ) : string
  if b then "True" else "False"

// showx(1)      ~>  int/showx(1)     (type is `int`)
// showx(3.14)   ~>  float64/showx(3.14)  (type is `float64`)
// showx('a')    ~>  char/showx('a')  (type is `char`)
fun example-basic() : console ()
  println(showx(1 : int))
  println(showx(3.14))
  println(showx('a'))
  println(showx(True))


// ─────────────────────────────────────────────────────────────────────────────
// Static overloading with tuple selectors
//
// We can overload field selectors for different tuple and record types.
// This is NOT possible with type classes, as type-class methods require an
// upfront class declaration.
// Note: we use `fstx` rather than `fst` to avoid shadowing stdlib `tuple/fst`.
// ─────────────────────────────────────────────────────────────────────────────

fun tuple/fstx( p : (a, b) ) : a
  p.fst

fun triple/fstx( p : (a, b, c) ) : a
  p.fst

// fstx((1, "a"))       ~>  tuple/fstx((1, "a"))
// fstx((1, "a", True)) ~>  triple/fstx((1, "a", True))
fun example-fst() : console ()
  println(fstx((1, "hello")).show)
  println(fstx((1, "hello", True)).show)


// ─────────────────────────────────────────────────────────────────────────────
// Elaboration with implicit parameters (combining both features)
//
// When a plain name needs an implicit parameter, static overloading first
// finds the right qualified definition, then the implicit is resolved.
// ─────────────────────────────────────────────────────────────────────────────

fun list/showx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(", ") ++ "]"

// showx([1,2,3])
//   1. static overloading: showx ~> list/showx  (type is `list<int>`)
//   2. implicit resolution: ?showx ~> int/showx  (type is `int -> string`)
//   elaborated to: list/showx([1,2,3], ?showx=int/showx)
fun example-list() : console ()
  println(showx([1, 2, 3]))
  println(showx(['a', 'b', 'c']))

// showx([[1],[2]])
//   elaborated to: list/showx([[1],[2]], ?showx=list/showx(_, ?showx=int/showx))
fun example-nested() : console ()
  println(showx([[1, 2], [3, 4]]))


// ─────────────────────────────────────────────────────────────────────────────
// Ambiguity: when the type context is not enough to disambiguate,
// the program is rejected.
//
// Functions in the `wrong/` namespace have their type errors demoted to
// warnings, so the file still compiles and demonstrates the rejection:
// ─────────────────────────────────────────────────────────────────────────────

fun wrong/ambiguous()
  showx([])      // rejected: element type unknown, cannot resolve `showx`

fun wrong/ambiguous2()
  showx          // rejected: would need type annotation to disambiguate


// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

fun main()
  println("Basic overloading:")
  example-basic()

  println("\nTuple/triple fst:")
  example-fst()

  println("\nList showx:")
  example-list()

  println("\nNested list showx:")
  example-nested()
