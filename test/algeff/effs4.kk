// test state as fun result from a handler (where resume is captured)

effect state<s> {
  fun get()    : s;
  fun set(x:s) : ()
}

val state = handler {
  return x -> fun(s) { x }     
  get()    -> fun(s) { resume(s)(s) }
  set(t)   -> fun(s) { resume(())(t) }
}

fun test() {
  set(2)
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  state(test)(0).println 
}