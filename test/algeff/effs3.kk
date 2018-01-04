// test name-clashes

effect state<s> {
  fun get()    : s
  fun set(x:s) : ()
}

val state = handler(s) {
  return x -> x
  get()    -> resume(s,s)
  set(t)   -> resume((),t)
}

fun test() {
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  state(0,test).println
}