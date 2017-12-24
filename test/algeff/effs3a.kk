// test locals with initializer

effect state<s> {
  fun get()    : s
  fun set(x:s) : ()
}

val state = handler(s=0) {
  return x -> x
  get()    -> resume(s,s)
  set(t)   -> resume((),t)
}

fun fstate(action) {
  handle(action)(s=0) {
    return x -> x
    get()    -> resume(s,s)
    set(t)   -> resume((),t)
  }
}

fun test() {
  if (get() > 0) then "positive" else "negative or zero"
}


fun main() {
  state(test).println
}