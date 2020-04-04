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

fun amb2(action) {
  with control flip { resume(False) + (resume(True) : list<_>) }
  action()
}

fun xor() {
  with p : bool = yield-bind(flip())
  with q = yield-bind(flip())
  p!=q
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

fun inc() : state<int> int {
  with i = yield-bind(get())
  with _x = yield-bind(set(i+1))
  i
}

fun xorst() {
  with p = yield-bind(xor())
  with i = yield-bind(inc())
  // with j:int = yield-bind(read())
  i
}

fun test-state-amb() : list<int> {  // 1,2,3,4
  //with reader(1)
  with state(0)
  with amb1
  xorst()
}

fun test-amb-state() : list<int> {  // 1,1,1,1
  //with reader(1)
  with amb1
  with state(0)
  xorst()
}

ambient val width : int


effect instance file<a> {
  fun read(x:a) : string
  fun fthrow(x:a) : b
}

effect rec cow {
  control moo() : (() -> cow ())
}
