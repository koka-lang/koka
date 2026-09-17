// Perceus (Parc) did not count a match *guard's* free variables as live, so a value
// used only in a guard was dropped before the match ran -- a use-after-free where the
// guard then read a freed (and reused) heap block.
//
// Here `x` is used only in the guard `x.sval == "hello"`. The scrutinee `Box2(junk)`
// allocates a same-shape value that reuses `x`'s just-freed block (whose string field
// then holds `junk`), so a miscompiling backend reads `junk` in the guard and prints False.

type box2
  Box2( s : string )
  Other2

fun sval( x : box2 ) : string
  match x
    Box2(s) -> s
    Other2  -> ""

noinline fun mkbox( s : string ) : box2
  Box2(s)

noinline fun test( x : box2, junk : string ) : bool
  match Box2(junk)                       // reuses x's just-freed block; its field becomes `junk`
    Box2(_) | x.sval == "hello" -> True  // `x` is used ONLY in the guard
    _       -> False

pub fun main()
  val x = mkbox("hello".repeat(1))          // heap Box2("hello"), refcount 1
  println(test(x, "wrongwrong".repeat(1)))  // x.sval is "hello" => must be True
