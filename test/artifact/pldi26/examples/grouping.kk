// grouping.kk — Grouping operations with structs and dot-unpacking
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 4.2: Grouping Operations
//
// When multiple operations logically belong together (like type-class methods),
// we can pass them as a single implicit struct parameter.
// Koka's `.?` dot-unpacking syntax unpacks the struct fields into scope.
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/grouping.kk

module grouping

import std/num/float64


// ─────────────────────────────────────────────────────────────────────────────
// Num<a>: grouping addition and multiplication
// ─────────────────────────────────────────────────────────────────────────────

struct num<a>
  plus : (a, a) -> a
  times : (a, a) -> a
  zero : a

val int/num : num<int> = Num(
  plus  = (+),
  times = (*),
  zero  = 0
)

val float64/num : num<float64> = Num(
  plus  = (+),
  times = (*),
  zero  = 0.0
)

// Without dot-unpacking: must match the struct to use its fields
fun fadd-explicit( x : a, y : a, z : a, ?num : num<a> ) : a
  match num
    Num(p, t, _) -> p(x, t(y, z))   // x + (y * z)

// With dot-unpacking (.?num): Koka automatically exposes `plus`, `times`, `zero`
// into scope, so we can write `x + (y * z)` directly.
fun fadd( x : a, y : a, z : a, .?num : num<a> ) : a
  plus(x, times(y, z))   // uses the unpacked `plus` and `times`

fun numx-sum( xs : list<a>, .?num : num<a> ) : a
  xs.foldl(zero, plus)

fun example-num() : console ()
  // fadd(2, 3, 4)  ~>  fadd(2, 3, 4, int/num)  =>  2 + (3 * 4) = 14
  println(fadd(2, 3, 4 : int).show)
  println(fadd(1.0, 2.0, 3.0 : float64).show)
  println(numx-sum([1, 2, 3, 4, 5] : list<int>).show)


// ─────────────────────────────────────────────────────────────────────────────
// Monoid<a>: grouping a zero and a combining operation
// ─────────────────────────────────────────────────────────────────────────────

value struct monoid<a>
  mzero : a
  mplus : (a, a) -> a

val intplus/monoid : monoid<int>    = Monoid(0,   (+))
val intmul/monoid  : monoid<int>    = Monoid(1,   (*))
val string/monoid  : monoid<string> = Monoid("",  (++))

fun mconcat( xs : list<a>, ?m/monoid : monoid<a> ) : a
  xs.foldl(m/monoid.mzero, m/monoid.mplus)

fun example-monoid() : console ()
  println(mconcat([1, 2, 3, 4, 5], ?m/monoid = intplus/monoid).show)  // 15
  println(mconcat([1, 2, 3, 4, 5], ?m/monoid = intmul/monoid).show)   // 120
  println(mconcat(["hello", " ", "world"], ?m/monoid = string/monoid)) // "hello world"


// ─────────────────────────────────────────────────────────────────────────────
// Monad<m>: grouping `pure` and `bind` with dot-unpacking
//
// The struct fields are named `pure` and `bind`; after `.?monad` dot-unpacking
// they become plain names in scope (just like type-class methods would be).
//
// monadplus<m> embeds a `base : monad<m>` field and adds a choice operation.
// The `default/monadplus/monad` derivation means: whenever a `?monad` is
// needed and a `?monadplus` is in scope, the `base` field is used automatically.
// ─────────────────────────────────────────────────────────────────────────────

value struct monad<m>
  pure : forall<a> (a -> m<a>)
  bind : forall<a,b> ((m<a>, a -> m<b>) -> m<b>)

value struct monadplus<m>
  base : monad<m>                       // the "base" monad
  plus : forall<a> (m<a>, m<a>) -> m<a>

// Default: derive monad from monadplus by extracting the `base` field.
// `base` is in scope because `.?monadplus` unpacks the struct.
fun default/monadplus/monad( .?monadplus : monadplus<m> ) : monad<m>
  base

val list/monad : monad<list>   = Monad( fn(x) [x],  fn(m, f) m.flatmap(f) )
val maybe/monad : monad<maybe> = Monad( Just,        fn(m, f) match m { Just(x) -> f(x); Nothing -> Nothing } )

val list/monadplus  : monadplus<list>  = Monadplus( list/monad,  (++) )
val maybe/monadplus : monadplus<maybe> = Monadplus( maybe/monad, fn(m1, m2) match m1 { Just -> m1; Nothing -> m2 } )

// sequence: uses `.?monad` dot-unpacking so `pure` and `bind` are plain names
fun sequence( ms : list<m<a>>, .?monad : monad<m> ) : m<list<a>>
  match ms
    Nil         -> pure([])
    Cons(m, mm) ->
      with x  <- bind(m)
      with xs <- bind(sequence(mm))
      pure(Cons(x, xs))

// mbind / mpure: thin wrappers that work without dot-unpacking at the call site
fun mbind( m : m<a>, f : a -> m<b>, .?monad : monad<m> ) : m<b>
  bind(m, f)

fun mpure( x : a, .?monad : monad<m> ) : m<a>
  pure(x)

// testtwice: takes monadplus; ?monad is resolved via default/monadplus/monad
fun testtwice( m : m<a>, .?monadplus : monadplus<m> ) : m<list<a>>
  with x <- mbind(plus(m, m))
  mpure([x])

fun example-monad() : console ()
  // sequence with maybe/monad
  match [Just(1), Just(2)].sequence
    Just(xs) -> println("maybe sequence: " ++ xs.show)
    Nothing  -> println("maybe sequence: Nothing")
  // sequence with list/monad
  println("list sequence: " ++ sequence([[1, 2], [3, 4]]).show)
  // testtwice uses list/monadplus; monad derived automatically from base field
  println("testtwice: " ++ [[1,2]].testtwice.concat.sequence.show)


// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

fun main()
  println("Num grouping:")
  example-num()
  println("\nMonoid grouping:")
  example-monoid()
  println("\nMonad grouping:")
  example-monad()
