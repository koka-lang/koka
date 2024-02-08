// Qualified names and disambiguation (overloading)
// Prerequisites:
//  - syntax/basic
module syntax/qualifiers

// We can always use fully qualified to disambiguate plain names.
// In VS Code, press `ctrl+alt` (or `ctrl+option` on the mac) to see
// disambiguation with inlay-hints.
fun example1()
  [1,2].map(show)

fun example1-explicit()
  [1,2].std/core/list/map(std/core/int/show)

// Qualified names can be shortened as well:
fun example1-semi-explicit()
  [1,2].list/map(core/int/show)

// We can define functions (or values) of the same name
// in the same module by using _local qualification_
fun int/foo(i)
  i.count-digits

fun string/foo(s)
  int/foo(s.parse-int.default(42))

// We can now use qualified names to disambiguate
// between the two top-level `foo` definitions:
fun example2()
  int/foo(10) + string/foo("hi")

// The Koka compiler uses type information
// to disambiguate names based on the argument types.
// This is more convenient than always writing full qualified names.
// (press `ctrl+alt` to see the inferred qualified names as inlay hints)
fun example3()
  foo(11) + foo("hi there")

// The disambiguation does not always succeed, and in such
// case you can either add more type information:
fun example4a(x : int)       // the type is required to resolve `foo(x)` to `int/foo(x)`
  foo(x) + foo("hi there")

// Or disambiguate manually by writing (enough of) the qualifier.
// This always works in Koka as the disambiguation is purely convenience.
// In Koka, a program with fully qualified names has an _untyped dynamic semantics_,
// i.e. we can execute a program without knowing any types (including effect handlers).
// This is in contrast to a system with type classes (like Haskell or Lean) where the
// types are essential (as one cannot disambiguate instances otherwise).
fun example4b(x)
  int/foo(x) + foo("hi there")   // without more type information, we need to qualify explicitly
