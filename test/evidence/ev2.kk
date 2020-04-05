public module ev2

import std/core/hnd

effect exc {
  control throw() : a
}

fun to-maybe( action ) {
  handle(action) {
    return x        -> Just(x)
    control throw() -> Nothing
  }
}

effect amb {
  fun flip() : bool
}

val amb1 = handler {
  return x { [x] }
  control flip { resume(False) + (resume(True) : list<_>) }
}


fun xor() {
  flip() != flip()
}

fun test-amb() {
  amb1(xor)
}

effect state<a> {
  fun get()    : a
  fun set(x:a) : ()
  //fun throw(s:string,x:a) : b
}

fun state(init : a, action : () -> <state<a>|e> b) : e b {
  var s := init
  handle( {mask<local>(action)} ) {
    fun get()  { s }
    fun set(x) { s := x }
  }
}

effect reader<a> {
  control read() : a
}

fun reader(init,action) {
  handle(action) {
    control read() { resume(init) }
  }
}

fun inc() : state<int> int {
  val i = get()
  set(i+1)
  i
}

fun xorst() {
  val p = xor()
  inc() + read()
}

fun xmap(xs : list<a>, f : a -> e b) : e list<b> {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),xx.xmap(f))
  }
}

fun xmap-acc(xs : list<a>, f : a -> e b, acc : list<b> ) : e list<b> {
  match(xs) {
    Nil -> acc.reverse
    Cons(x,xx) -> xx.xmap-acc(f,Cons(f(x),acc))
  }
}


fun test-state-amb() : list<int> {  // 1,2,3,4
  with reader(1)
  with state(0)
  with amb1
  xorst()
}

fun test-amb-state() : list<int> {  // 1,1,1,1
  with reader(1)
  with amb1
  with state(0)
  xorst()
}

fun test() {
  test-amb().show-list(show).println
  //test-state().println
  //test-difficult().println
  test-state-amb().show-list(show).println
  test-amb-state().show-list(show).println
}

ambient val width : int


effect instance file<a> {
  fun read(x:a) : string
  fun fthrow(x:a) : b
}

effect rec cow {
  control moo() : (() -> cow ())
}
