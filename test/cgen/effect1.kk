
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

fun state(init, action) {
  var s := init
  handle({mask<local>(action)}) {
    return x -> (x,s)
    ctl get()    -> { resume(s) }
    ctl put(x)   -> { s := x; resume(()) }
  }
}

fun test() {
  state(2,counter).fst
}
