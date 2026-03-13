// intro.kk — Introduction examples from the paper
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 1: Introduction
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/intro.kk
//
// In VS Code, press Ctrl+Alt (Ctrl+Option on macOS) to see inferred implicits
// displayed as inlay hints.

module intro

import std/num/float64
import std/core/undiv


// ─────────────────────────────────────────────────────────────────────────────
// Syntactic implicit parameters: show-int with ?base
//
// The function takes an implicit parameter `base` denoted by `?`.
// At each call site, the name `base` is resolved in the caller's scope.
// ─────────────────────────────────────────────────────────────────────────────

fun show-digit( x : int ) : string
  if x < 10 then x.show else [('a' + (x - 10).char)].string

fun show-int( x : int, ?base : int ) : string
  if x < base
    then show-digit(x)
    else show-int((x / base).pretend-decreasing) ++ show-digit(x % base)
    //   ^^^^^^^^^^^^^^^^^^^
    // The recursive call does not pass `base` explicitly.
    // It is supplied implicitly as the `base` in scope (the parameter itself).
    // Elaborated to: show-int(x / base, ?base=base)

fun example-show-int() : string
  val base = 10
  show-int(42)
  // Elaborated to: show-int(42, ?base=base)   =>  "42"

fun example-show-binary() : string
  val base = 2
  show-int(42)
  // Elaborated to: show-int(42, ?base=base)   =>  "101010"

fun example-show-hex() : string
  val base = 16
  show-int(255)
  // Elaborated to: show-int(255, ?base=base)  =>  "ff"


// ─────────────────────────────────────────────────────────────────────────────
// Static overloading: qualified `showx` definitions
//
// We use the name `showx` (rather than `show`) to avoid shadowing the stdlib's
// own `show` definitions, which would cause ambiguity. The paper uses `show`
// as the illustrative name; this is a minor renaming for the artifact.
// ─────────────────────────────────────────────────────────────────────────────

fun int/showx( x : int ) : string
  show-int(x, 10)

fun float64/showx( f : float64 ) : string
  std/num/float64/show(f)

fun bool/showx( b : bool ) : string
  if b then "True" else "False"

fun char/showx( c : char ) : string
  "'" ++ c.string ++ "'"


// ─────────────────────────────────────────────────────────────────────────────
// Combining implicit parameters and static overloading
//
// The `list/showx` function uses a `?showx` implicit parameter.
// At the call site `showx([1])`:
//   1. Static overloading elaborates `showx` to `list/showx` (type is `list<int>`)
//   2. The `?showx` implicit is resolved to `int/showx` (type is `int -> string`)
//
// Note: the paper presents a recursive match for illustration (to show that
// the implicit is threaded automatically in the recursive call). Here we use
// `map`+`join` to keep the implementation total.
// ─────────────────────────────────────────────────────────────────────────────

fun list/showx( xs : list<a>, ?showx : a -> string ) : string
  "[" ++ xs.map(showx).join(", ") ++ "]"

// showx([1])
//   elaborated to: list/showx([1], ?showx=int/showx)
fun example-list-int() : string
  showx([1, 2, 3])

// showx([[1],[2]])
//   elaborated to: list/showx([[1],[2]], ?showx=list/showx(_, ?showx=int/showx))
fun example-list-list() : string
  showx([[1, 2], [3, 4]])

// showx([True, False])
//   elaborated to: list/showx([True, False], ?showx=bool/showx)
fun example-list-bool() : string
  showx([True, False])


// ─────────────────────────────────────────────────────────────────────────────
// Tuple show: multiple implicit parameters with qualified names
//
// We use qualified implicit parameter names `?fst/showx` and `?snd/showx`
// to distinguish showing each component, while at each call site both
// are resolved as the plain name `showx` from the local context.
// ─────────────────────────────────────────────────────────────────────────────

fun tuple/showx( p : (a, b), ?fst/showx : a -> string, ?snd/showx : b -> string ) : string
  "(" ++ fst/showx(p.fst) ++ ", " ++ snd/showx(p.snd) ++ ")"

// showx((1, True))
//   elaborated to: tuple/showx((1, True), ?fst/showx=int/showx, ?snd/showx=bool/showx)
fun example-tuple() : string
  showx((1, True))

// showx([(1, True), (2, False)])
//   elaborated to: list/showx([(1,True),(2,False)],
//                    ?showx=tuple/showx(_, ?fst/showx=int/showx, ?snd/showx=bool/showx))
fun example-list-of-tuples() : string
  showx([(1, True), (2, False)])

// showx([([1], True)])
//   elaborated to: list/showx([([1],True)],
//                    ?showx=tuple/showx(_, ?fst/showx=list/showx(_, ?showx=int/showx), ?snd/showx=bool/showx))
fun example-nested() : string
  showx([([1, 2], True)])


// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

fun main() : console ()
  // Basic implicit parameter
  println("show-int examples:")
  println("  decimal: " ++ example-show-int())
  println("  binary:  " ++ example-show-binary())
  println("  hex:     " ++ example-show-hex())

  // Static overloading
  println("\nStatic overloading:")
  println("  int:   " ++ showx(42 : int))
  println("  float: " ++ showx(3.14 : float64))
  println("  bool:  " ++ showx(True))
  println("  char:  " ++ showx('x'))

  // Combining implicits and overloading
  println("\nList show:")
  println("  " ++ example-list-int())
  println("  " ++ example-list-list())
  println("  " ++ example-list-bool())

  // Tuple show
  println("\nTuple show:")
  println("  " ++ example-tuple())
  println("  " ++ example-list-of-tuples())
  println("  " ++ example-nested())
