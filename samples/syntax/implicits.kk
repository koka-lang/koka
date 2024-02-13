// Experimental feature: Implicit parameters
// This is still in design and may change.
//
// In essence, implicits are just regular parameters that can usually be provided by the compiler.
//
// The main idea is to provide a simple mechanism that can help with common overloading situations,
// like equality, show, or container operations, but trying to avoid the complexity and large
// design space of type classes. Koka aims to be the C of FP -- not the C++ :-)
//
// Prerequisites:
//  - syntax/basic
//  - syntax/qualifiers
module syntax/implicits

import std/num/float64

// Suppose we have some top-level equality definitions
// where we use local qualification to disambiguate them

fun int/eq( x : int, y : int ) : bool
  (x == y)

fun float64/eq( x : float64, y : float64 ) : bool
  (x == y)

fun char/eq( x : char, y : char ) : bool
  (x == y)

// And we have an equality function on lists as well:
fun list-eq-explicit( xs : list<a>, ys : list<a>, eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> list-eq-explicit(xx,yy,eq)
      _ -> False
    Nil -> ys.is-nil


fun example1()
  list-eq-explicit([1,2],[1,2],eq)  // Koka disambiguates `eq` to `int/eq` (since we use `:int`` lists)

// It can be cumbersome to always pass in the explicit `eq` function.
// We can instead make it an _implicit_ parameter, written as `?eq`.
// Such parameter is automatically resolved by the compiler
// to _any `eq` function at the call site_.
// (where the argument types may need to be instantiated enough to disambiguate)
//
// In other words, any call
//  `list-eq-explicit(_,_)`
// is essentially expanded to
//  `list-eq-explicit(_,_,eq)`
//
// So, in the recursive call below, `list-eq(xx,yy)`, the implicit
// parameter is resolved to the local `eq` parameter itself.
// (in VS Code, press `ctrl+alt` to see the implicits as inlay hints)
fun list-eq( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> list-eq(xx,yy)
      _ -> False
    Nil -> ys.is-nil

// In this example, the implicit `?eq` parameter is resolved to `int/eq`.
fun example2()
  list-eq( [1,2], [1,2] )

// Sometimes, the implicit parameter can still not be disambiguated.
// In that case we can either use a local definition or an implicit parameter again.
// (ironically, the implicit parameters themselves must be declared explicitly :-))
fun eq-refl(xx, ?eq)
  list-eq(xx,xx)

// Or we can provide the implicit argument explicitly.
// (this would be like a named instance with type classes)
fun example3()
  list-eq( [], [], ?eq = int/eq )

// or sneakily
fun example3a()
  list-eq( [1,2], [1,2], ?eq = fn(x,y) !int/eq(x,y) )


// The `?eq` notation is actually a shorthand for a locally qualified name `@implicit/eq`.
// When resolving an implicit parameter `?eq`, the compiler looks for the plain name `eq` though.
// This means that we can still refer to `eq` definitions at the top-level scope.
// For example, we can write list equality also as:
fun list/eq( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> eq(xx,yy)   // ~> Cons(y,yy) | ?eq(x,y) -> list/eq(xx,yy, ?eq = ?eq)
      _ -> False
    Nil -> ys.is-nil

// Above, the first `eq(x,y` resolves to `?eq(x,y)` (== `@implicit/eq(x,y)`),
// the second `eq(xx,yy)` resolves to `list/eq(xx,yy)`, bypassing
// the local `?eq` parameter (as its fully qualified name is `?eq` (==`@implicit/eq`)).
// The implicit parameter to `list/eq` is resolved to the local parameter `?eq` again.
// (these are just the regular rules for disambiguating qualified names based on types -- nothing specific to implicits)
fun example4()
  eq([1,2],[1,2])


// Implicit parameters are resolved recursively.
// Below, the implicit `?eq` is resolved first to `list/eq`, and
// then the implicit `?eq` of `list/eq` is resolved further to `int/eq`.
// (again, in VS Code press `ctrl+alt` to see the inlay hints)
fun example5()
  eq( [[1,2],[]], [[1,2],[]] )


// # Experimental territory

// When resolving an implicit parameter, the compiler also considers
// matching functions that take further implicit parameters themselves.
// These are called _conversions_. For example, given a `compare` function,
// we could provide an `eq` function:
fun compare/eq( x : a, y : a, ?compare : (a,a) -> order )
  compare(x,y) == Eq

fun string/compare( x : string, y : string ) : order
  cmp(x,y)

// In the following example, the implicit `?eq` parameter is first
// resolved to `compare/eq`, and then recursively `?compare` to `string/compare`.
fun example6()
  eq( ["hi"], ["there"] )


// These conversions can lead to ambiguities. Consider adding an `:int` comparison.
fun int/compare( x : int, y : int ) : order
  cmp(x,y)

// We can now choose to resolve `?eq` either as `int/eq`, or `compare/eq(_,_,?compare=?int/compare)`
// The compiler disambiguates always using the _shortest chain_ (i.e. `int/eq`)
fun example7()
  eq([1,2],[1,2])       // ?eq=int/eq

// However, any conversion chain ending in (the most) local parameters still takes precedence.
// (so the full disambiguation rule is _most locals_, _shortest chain_)
fun test1(?compare)
  eq([1,2],[1,2])       // ?eq=compare/eq(_,_,?compare=?compare)


// Sometimes we need multiple implicit parameters resolving to the same name.
// In that case we can further qualify the local names. This way each local parameter
// has a unique qualified name, but each one is resolved using the plain `eq` name at the call-site.
// (again, these are the normal disambiguation rules for qualified names, not specific for implicits)
fun tuple/eq( (x1,y1) : (a,b), (x2,y2) : (a,b), ?a/eq : (a,a) -> bool, ?b/eq : (b,b) -> bool ) : bool
  eq(x1,x2) && eq(y1,y2)

// Or without types and explicit qualification
fun tuple/eq2( (x1,y1), (x2,y2), ?a/eq, ?b/eq ) : bool
  a/eq(x1,x2) && b/eq(y1,y2)

// As we can see, implicit parameters can really help reduce boilerplate!
fun example8()
  eq(([1],"a"),([1],"a"))   // `?a/eq=list/eq(_,_,int/eq),?b/eq=compare/eq(_,_,string/compare)`

pub fun main()
  example8().println

// see `test/overload` for further examples.