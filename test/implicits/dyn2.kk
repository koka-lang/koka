// --------------------------------------------------------
//  Examples of common effects using dynamic and with
// --------------------------------------------------------
// public module common

import std/os/readline

// --------------------------------------------------------
//  Exceptions
// --------------------------------------------------------

implicit fun raise(s : string ) : a

fun safe-div(x,y) {
  if (y==0) then raise("division by zero") else x / y
}

fun rcatch(action,h) {
  with effect raise(s) { h(s) }
  action()
}

fun zerodiv(x,y) {
  rcatch( { safe-div(x,y) }, fun(s) { 0 } )
}

fun to-maybe(action) {
  with effect raise(_s) { Nothing }
  Just(action())
}

fun test1() {
  zerodiv(10,0).println
}


// --------------------------------------------------------
//  Input
// --------------------------------------------------------

implicit fun input() : string

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

implicit state<s> {
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

val state-param = handler(s) {
  return x -> (x,s)
  get()    -> resume(s,s)
  put(s')  -> resume((),s')
}

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

implicit<a> fun yield( item : a ) : ()

fun iterate(xs : list<a> ) : yield<a> () {
  match(xs) {
    Nil -> ()
    Cons(x,xx) -> { yield(x); iterate(xx) }
  }
}

fun foreach( f : a -> e bool, action : () -> <yield<a>|e> () ) : e () {
  with effect yield(x) { if (f(x)) then resume(()) else () }
  action()
}

fun test4() {
  foreach fun(x:int) {
    println(x)
    (x<=1)
  }
  { [1,2,3].iterate }
}

fun test4b() {
  with return x { () }
  with amb
  with foreach( fun(x:int) { if (flip()) then println(x) else println("flip false " + x.show); (x<=1) } )
  [1,2,3].iterate
}

// --------------------------------------------------------
//  Resume more than once
// --------------------------------------------------------

implicit fun flip() : bool

fun xor() : flip bool {
  val p = flip()
  val q = flip()
  (p||q) && not(p&&q)
}

val coinflip = with fun flip() { random-bool() }

val amb = with {
  return x      { [x] }
  effect flip() { resume(False) + resume(True) }
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
  fun(err) {
    println("error: " + err )
  }
}


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
  xs.show-list(fun(x) { x.show-tuple(show,show) })
}

fun show( x : (list<bool>,int) ) : string  {
  x.show-tuple(show,show)
}
