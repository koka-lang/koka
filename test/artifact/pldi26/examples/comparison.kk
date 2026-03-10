// comparison.kk — Equality, comparison, and the default namespace
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 4 (Practice): Default implementations and the `default/` namespace
//
// Note: we use `cmpx` / `(===)` / `(!==)` as names rather than `cmp` / `(==)` / `(!=)`
// to avoid shadowing stdlib definitions with identical qualified names
// (e.g. `int/cmp`, `int/(==)` are already defined in std/core). The paper uses
// these plain names illustratively; the semantics are identical.
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/comparison.kk

module comparison


// ─────────────────────────────────────────────────────────────────────────────
// Comparison functions: one definition per base type + generic list comparison
// ─────────────────────────────────────────────────────────────────────────────

fun int/cmpx( x : int, y : int ) : order
  std/core/int/cmp(x, y)

fun char/cmpx( x : char, y : char ) : order
  std/core/int/cmp(x.int, y.int)

fun list/cmpx( xs : list<a>, ys : list<a>, ?cmpx : (a, a) -> order ) : order
  match xs
    Nil         -> match ys { Nil -> Eq; _ -> Lt }
    Cons(x, xx) -> match ys
      Nil         -> Gt
      Cons(y, yy) -> match cmpx(x, y)
        Eq  -> list/cmpx(xx, yy)
        ord -> ord

fun order/showx( o : order ) : string
  match o
    Lt -> "Lt"
    Eq -> "Eq"
    Gt -> "Gt"


// ─────────────────────────────────────────────────────────────────────────────
// Equality derived from comparison via the `default/` namespace
//
// `default/(===)` is in the `default/` namespace, which has scope depth -1.
// This means any user-defined `foo/(===)` at scope depth 0 takes precedence,
// without causing an ambiguity error.
// ─────────────────────────────────────────────────────────────────────────────

fun default/(===)( x : a, y : a, ?cmpx : (a, a) -> order ) : bool
  match cmpx(x, y)
    Eq -> True
    _  -> False

// A faster primitive equality for integers.
// Since this is at scope depth 0 and `default/(===)` is at -1, `int/(===)` wins.
fun int/(===)( x : int, y : int ) : bool
  (x == y)


// ─────────────────────────────────────────────────────────────────────────────
// Inequality derived from equality
// ─────────────────────────────────────────────────────────────────────────────

fun (!==)( x : a, y : a, ?(===) : (a, a) -> bool ) : bool
  !(===)(x, y)


// ─────────────────────────────────────────────────────────────────────────────
// Main: demonstrate comparison, default/(===), int/(===) disambiguation, (!=)
// ─────────────────────────────────────────────────────────────────────────────

fun main()
  // int/cmpx, char/cmpx, list/cmpx
  println(order/showx(cmpx(1, 2)))          // int/cmpx        => Lt
  println(order/showx(cmpx('a', 'z')))      // char/cmpx       => Lt
  println(order/showx(cmpx([1,2],[1,3])))   // list/cmpx(_, ?cmpx=int/cmpx)  => Lt

  // default/(===) derives equality from cmpx; int/(===) is preferred for int
  println((1 === 2).show)           // int/(===)(_, _)  => False
  println(([1,2] === [1,2]).show)   // default/(===)(_, _, ?cmpx=list/cmpx(_, ?cmpx=int/cmpx))  => True

  // (!=) uses implicit ?(===)
  println((1 !== 2).show)           // (!==)(_, _, ?(===)=int/(===))  => True
