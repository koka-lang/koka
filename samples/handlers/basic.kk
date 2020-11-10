/* Basic effect handler examples
   Prerequisites:
   - syntax/basic
   - syntax/var
   - syntax/with
*/

module basic

import std/num/random

// --------------------------------------------------------
//  Exceptions
// --------------------------------------------------------

// An effect `:raise` with a single `control` operation `raise`.
// This is equivalent to writing:
// ```
// effect raise {
//   control raise(s:string) : a
// }
// ```
effect control raise(s : string ) : a

fun safe-div(x,y) {
  if (y==0) then raise("division by zero") else x / y
}

fun rcatch(action,h) {
  with control raise(s){ h(s) }     // handle `raise` with function `h`
  action()                          // in `action()`
}

fun zerodiv(x,y) {
  rcatch( { safe-div(x,y) }, fn(s){ 0 } )
}

fun to-maybe(action) {
  with control raise(_s) { Nothing }
  Just(action())
}

pub fun test1() {
  zerodiv(10,0).println
}


// --------------------------------------------------------
//  Input
//  Essentially models dynamically bound state
// --------------------------------------------------------

// Declare an effect `:input` with a single operation `input`.
effect fun input() : string

fun hello() {
  val name = input()
  println("Hello " + name + ", " + input())  // "Hello <input>, <input>"
}

pub fun test2() {
  with fun input() { "there" }   // handle input; a `fun` operation resumes implicitly
  hello()
}


// --------------------------------------------------------
//  State
// --------------------------------------------------------

effect state<s> {
  fun get() : s
  fun put( x : s ) : ()
}

fun counter() {
  val i = get()
  if (i <= 0) then () else {
    println("hi")
    put(i - 1);
    counter()
  }
}


// State handler.
fun state(init, action) {
  var s := init              // use a local mutable variable
  with {
    return x  { (x,s) }
    fun get() { s }
    fun put(x){ s := x }
  }
  action()
}

pub fun test3() {
  state(1,counter).snd
}


// --------------------------------------------------------
//  Iterators
// --------------------------------------------------------

effect<a> control yield( item : a ) : ()

fun iterate(xs : list<a> ) : yield<a> () {
  match(xs) {
    Nil -> ()
    Cons(x,xx) -> { yield(x); iterate(xx) }
  }
}

fun foreach( f : a -> e bool, action : () -> <yield<a>|e> () ) : e () {
  with control yield(x) { if (f(x)) then resume(()) else () }
  action()
}

pub fun test4() {
  foreach fn(x:int) {
    println(x)
    (x<=1)
  }
  { [1,2,3].iterate }
}

fun test4b() {
  with return (x){ () }
  with amb
  with foreach fn(x:int) {
    if (flip()) then println(x)
                else println("flip false " + x.show)
    (x<=1)
  }
  [1,2,3].iterate
}

// --------------------------------------------------------
//  Resume more than once
// --------------------------------------------------------

effect control flip() : bool

fun xor() : flip bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}

// handler that randomly flips
val coinflip = handler {
  fun flip(){ srandom-bool() }
}

// handler that returns all possible outcomes
val amb = handler {
  return x      { [x] }
  control flip(){ resume(False) + resume(True) }
}

pub fun test5() {
  amb(xor).show.println
}


// --------------------------------------------------------
//  Combine effects
// --------------------------------------------------------

fun surprising() : <flip,state<int>> bool {
  val p = flip()
  val i = get()
  put(i+1)
  if (i>0 && p) then xor() else False
}

pub fun test6() {
  state(0){ amb(surprising) }.show.println
}

pub fun test7() {
  amb{ state(0,surprising) }.show.println
}

// --------------------------------------------------------
//  Testing
// --------------------------------------------------------
pub fun main() {
  test1()
  test2()
  test3()
  test4()
  test5()
  test6()
  42
}

fun show( xs : list<(bool,int)> ) : string  {
  xs.show-list(fn(x){ x.show-tuple(show,show) })
}

fun show( x : (list<bool>,int) ) : string  {
  x.show-tuple(show,show)
}
