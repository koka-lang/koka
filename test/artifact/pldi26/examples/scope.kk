// scope.kk — Scope-based disambiguation
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 3.5: Scope Based Disambiguation
//
// The [var-scope] rule: prefer definitions from inner scopes over outer ones.
// This allows local overriding without ambiguity errors.
//
// Note: we use `showx`, `cmpx`, `(===)` rather than `show`, `cmp`, `(==)` to
// avoid shadowing stdlib definitions with identical qualified names. The paper
// uses plain names illustratively; the semantics are identical.
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/scope.kk

module scope


// ─────────────────────────────────────────────────────────────────────────────
// Global (outer scope, depth 0) definitions
// ─────────────────────────────────────────────────────────────────────────────

fun int/showx( x : int ) : string
  x.show

fun list/showx( xs : list<a>, ?showx : a -> e string ) : e string
  "[" ++ xs.map(showx).join(", ") ++ "]"


// ─────────────────────────────────────────────────────────────────────────────
// Inner-scope override
//
// A locally defined `myint/showx` lives at a deeper scope than `int/showx`.
// The [var-scope] rule prefers it over `int/showx` — no ambiguity error.
// ─────────────────────────────────────────────────────────────────────────────

fun secret-list() : string
  fun myint/showx(i : int) : string
    "*"                          // hide integer values
  showx([1, 2, 3])
  // elaborated to: list/showx([1,2,3], ?showx=myint/showx)
  // NOT int/showx, because myint/showx is in a deeper scope

fun example-local-override() : console ()
  println(secret-list())                       // "[*, *, *]"
  println(showx([1, 2, 3]))                    // "[1, 2, 3]"  (using int/showx)


// ─────────────────────────────────────────────────────────────────────────────
// Implicit parameters capture their scope
//
// When an implicit `?showx` is in scope, it is preferred over global `int/showx`
// for the resolution of further implicits.
// ─────────────────────────────────────────────────────────────────────────────

fun showx-with( xs : list<a>, ?showx : a -> e string ) : e string
  showx(xs)
  // `showx(xs)` elaborates to `list/showx(xs, ?showx=?showx)`.
  // The caller-supplied `?showx` parameter lives at a deeper scope than any
  // global `foo/showx`, so [var-scope] picks it up automatically — no ambiguity.

fun example-implicit-capture() : console ()
  println(showx-with([1, 2, 3], fn(x) "(" ++ x.show ++ ")"))
  // shows: "[(1), (2), (3)]"


// ─────────────────────────────────────────────────────────────────────────────
// The `default/` namespace: scope depth -1
//
// Definitions in `default/` are treated as coming from a scope *outside*
// the global module scope (depth -1). Any regular definition (depth >= 0)
// takes precedence without causing an ambiguity error.
// ─────────────────────────────────────────────────────────────────────────────

fun int/cmpx( x : int, y : int ) : order
  std/core/int/cmp(x, y)

fun char/cmpx( x : char, y : char ) : order
  std/core/int/cmp(x.int, y.int)

// `default/(===)` is at scope depth -1: it will lose to any user `foo/(===)`
fun default/(===)( x : a, y : a, ?cmpx : (a, a) -> order ) : bool
  match cmpx(x, y)
    Eq -> True
    _  -> False

// A more efficient equality for `int`: this wins over `default/(===)` since
// scope depth 0 > -1. No ambiguity error.
fun int/(===)( x : int, y : int ) : bool
  (x == y)

fun example-default() : console ()
  // int/(===) wins over default/(===) for integers: scope depth 0 > -1
  println((1 === 2).show)        // False — via int/(===)
  // char has no specific equality, so default/(===) is used with char/cmpx
  println(('a' === 'b').show)    // False — via default/(===)(char/cmpx)


// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

fun main()
  println("Inner-scope override:")
  example-local-override()

  println("\nImplicit captures inner scope:")
  example-implicit-capture()

  println("\nDefault namespace (depth -1):")
  example-default()
