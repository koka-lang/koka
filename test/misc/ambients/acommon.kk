// --------------------------------------------------------
//  Examples of common effects
// --------------------------------------------------------
// pub module common

// import std/os/readline
import std/num/random

// --------------------------------------------------------
//  Exceptions
// --------------------------------------------------------

ambient control raise(s : string ) : a

fun safe-div(x,y) {
  if (y==0) then raise("division by zero") else x / y
}

fun rcatch(action,h) {
  with control raise(s) { h(s) }
  action()
}

fun zerodiv(x,y) {
  rcatch( { safe-div(x,y) }, fn(s) { 0 } )
}

fun to-maybe(action) {
  with control raise(_s) { Nothing }
  Just(action())
}

fun test1() {
  zerodiv(10,0).println
}


// --------------------------------------------------------
//  Input
// --------------------------------------------------------

ambient fun input() : string

fun hello() {
  val name = input()
  println("Hello " + name + ", " + input())
}

fun test2() {
  with fun input() { "there" }
  hello()
}

// --------------------------------------------------------
//  State
// --------------------------------------------------------

ambient state<s> {
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

// using parameterized state
/*
val state-param = handler(s) {
  return x -> (x,s)
  get()    -> resume(s,s)
  put(s')  -> resume((),s')
}
*/

// using builtin local variables
fun state(init, action) {
  var s := init
  with {
    return x   { (x,s) }
    fun get()  { s }
    fun put(x) { s := x }
  }
  action()
}

fun test3() {
  state(1,counter).snd
}


// --------------------------------------------------------
//  Iterators
// --------------------------------------------------------

ambient<a> control yield( item : a ) : ()

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

fun test4() {
  foreach fn(x:int) {
    println(x)
    (x<=1)
  }
  { [1,2,3].iterate }
}

fun test4b() {
  with return(x){ () }
  with amb
  with foreach( fn(x:int) { if (flip()) then println(x) else println("flip false " + x.show); (x<=1) } )
  [1,2,3].iterate
}

// --------------------------------------------------------
//  Resume more than once
// --------------------------------------------------------

ambient control flip() : bool

fun xor() : flip bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}

val coinflip = handler { fun flip() { srandom-bool() } }

val amb = handler {
  return x      { [x] }
  control flip() { resume(False) + resume(True) }
}

fun test5() {
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

fun test6() {
  (state(0){amb(surprising)}).show.println
}

fun test7() {
  (amb{state(0,surprising)}).show.println
}


// --------------------------------------------------------
//  An asynchronous effect
// --------------------------------------------------------
/*
fun ask-age() {
  println("what is your name?")
  val name = readline()           // asynchronous!
  println("and you age?")
  val age = readline()
  println("hello " + name + ", you are " + age)
}


fun ask-age-err() {
  rcatch {
    println("what is your name?")
    val name = readline()
    raise("ouch!")
    println("hello " + name)
  }
  fn(err) {
    println("error: " + err )
  }
}
*/

// --------------------------------------------------------
//  Testing
// --------------------------------------------------------
fun main() {
  test1()
  test2()
  test3()
  test4()
  test5()
  test6()
  test7()
  ()
}

fun show( xs : list<(bool,int)> ) : string  {
  xs.show-list(fn(x) { x.show-tuple(show,show) })
}

fun show( x : (list<bool>,int) ) : string  {
  x.show-tuple(show,show)
}
