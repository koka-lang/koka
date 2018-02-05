// --------------------------------------------------------
//  Examples of common effects. All from the paper.
//  Exceptions, state, iterators, ambiguity, and asynchronous effects.
// --------------------------------------------------------
public module common

import std/os/readline

// --------------------------------------------------------
//  Exceptions
// --------------------------------------------------------

effect exc {
  fun raise(s : string ) : a
}

fun safe-div(x,y) {
  if (y==0) then raise("division by zero") else x / y
}

fun catch(action,h) {
  handle(action) {
    raise(s) -> h(s)
  }
}

fun zerodiv(x,y) {
  common/catch( { safe-div(x,y) }, fun(s) { 0 } )
}

val to-maybe = handler {
  return x -> Just(x)
  raise(s) -> Nothing
}

fun test1() {
  zerodiv(10,0).println
}


// --------------------------------------------------------
//  Input
// --------------------------------------------------------

effect input {
  fun getstr() : string
}

fun hello() {
  val name = getstr()
  println("Hello " + name + ", " + getstr())
}

val always-there = handler {
  getstr() -> resume("there")
}

fun test2() {
  always-there(hello)
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

val state = handler(s) {
  return x -> (x,s)
  get()    -> resume(s,s)
  put(s')  -> resume((),s')
}

fun test3() {
  state(2,counter)
}


// --------------------------------------------------------
//  Iterators
// --------------------------------------------------------

effect yield<a> {
  fun yield( item : a ) : ()
}

fun iterate(xs : list<a> ) : yield<a> () {
  match(xs) {
    Nil -> ()
    Cons(x,xx) -> { yield(x); iterate(xx) }
  }
}

// note: due to a technicality in cps translation we need to specify 'incpsx' for now. to be fixed soon.
fun foreach( f : a -> e bool, action : () -> <yield<a>|e> () ) : e () {
  handle(action) {
    return x -> ()
    yield(x) -> if (f(x) /*incps1(f,x)*/) then resume(()) else ()
  }
}

fun test4() {
  foreach fun(x:int) { 
    println(x)
    (x<=1) 
  }
  { [1,2,3].iterate } 
}

fun test4b() {
  amb{ foreach( fun(x:int) { if (flip()) then println(x) else println("false"); (x<=1) }, { [1,2,3].iterate } ) }
}

// --------------------------------------------------------
//  Resume more than once
// --------------------------------------------------------

effect amb {
  fun flip() : bool
}
 
fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

val coinflip = handler {
  flip() -> resume(random-bool())
}

val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

fun test5() {
  amb(xor).show.println
}


// --------------------------------------------------------
//  Combine effects
// --------------------------------------------------------

fun surprising() : <amb,state<int>> bool {
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
  catch {
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