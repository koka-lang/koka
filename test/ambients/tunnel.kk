// -------------------------------------------------------
//  Examples from the paper "Abstraction-Safe Effect Handlers via Tunneling"
//  By Yizhou Zhang, and Andrew C Myers, POPL 2019
// -------------------------------------------------------

type tree<a> {
  con Bin(value:a,left:tree<a>, right:tree<a>)
  con Leaf
}

ambient<a> control yield(value:a) : ()

// An in-order iterator for trees
fun iterate(t) {
  match(t) {
    Leaf -> ()
    Bin(x,l,r) {
      iterate(l)
      yield(x)
      iterate(r)
    }
  }
}

// Example tree
fun tip(x) { Bin(x,Leaf,Leaf)}
val tree5 = Bin(2,tip(1),Bin(4,tip(3),tip(5)))

// Example from Figure 1
fun test1() {
  with fun yield(x:int) { print(x) }
  tree5.iterate
}

// print at most 4 elements
fun print4(action) {
  var count := 0
  with control yield(x:int) {
    if (count >= 4) then () else {
      count := count + 1
      print(x)
      resume(())
    }
  }
  action()
}

fun test2() {
  with print4
  tree5.iterate
}

/* Iterator that takes a predicate `f` (Page 4)
Note: we mask here to not handle a possible `:yield` effect in the predicate `f`.
In `fiterate1` we left out the `mask`. Notice that each fun has a *different* type:
```
  fun fiterate( tree<a>, f : (a) -> e bool ) : <yield<a>|e> ()
  fun fiterate1( tree<a>, f : (a) -> <yield<a>|e> bool ) : <yield<a>|e> ()
```
The type reflects that `fiterate1` may handle the `:yield` effect of the predicate,
while `fiterate` encapsulates the predicate and is polymorphic it the effect of
the predicate. In the paper, the encapsulation is done by default which is often
what we want (but not always!! (like writing a `catch` fun for example)).

Note 1. We need to use `mask` now but perhaps it is
possible to reverse this behaviour and use it by default, with an `unmask` if
we want the other behaviour? This would mimic the tunneling solution.

Note 2: In contrast as claimed in the paper, Reynold's parametricity *does* hold as
both functions have a different type.
*/
fun fiterate(t,f) {
  match(t) {
    Leaf -> ()
    Bin(x,l,r) {
      l.fiterate(f)
      if (mask<yield>{f(x)}) then yield(x)
      r.fiterate(f)
    }
  }
}

fun fiterate1(t,f) {
  match(t) {
    Leaf -> ()
    Bin(x,l,r) {
      l.fiterate1(f)
      if (f(x)) then yield(x)
      r.fiterate1(f)
    }
  }
}

// Implementation of the size of a tree that uses `yield` internally. (Figure 2)
// Inferred type: `forall<a,e> (t : tree<a>, f : (a) -> e bool) -> e int`
// The type shows that this is safely encapsulated as the predicate `f`
// is again polymorpic in `e`.
fun fsize1(t,f) {
  var count := 0
  (with fun yield(_x) { count := count + 1 } in fiterate(t,f))
  count
}

// Implementation of the size of a tree without using `yield` internally. (Figure 2)
// Inferred type: `forall<a,e> (t : tree<a>, f : (a) -> e bool) -> e int`
fun fsize2(t,f) {
  match(t) {
    Leaf -> 0
    Bin(x,l,r) {
      val lc = l.fsize2(f)
      val rc = r.fsize2(f)
      val c  = if (f(x)) then 1 else 0
      c + lc + rc
    }
  }
}

// Use either implementation (Figure 3)
fun test3a() {
  val fsize = fsize1
  fun g(x:int) { yield(x); even?(x) }
  with fun yield(x:int) { print(x) }
  tree5.fsize(g)
}

fun test3b() {
  val fsize = fsize2
  fun g(x:int) { yield(x); even?(x) }
  with fun yield(x:int) { print(x) }
  tree5.fsize(g)
}

fun main() {
  println( ", even count a: " + test3a().show )
  println( ", even count b: " + test3b().show )
}
