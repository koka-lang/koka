// test state as fun result from a handler (where resume is captured)

effect state<s> {
  get()    : s;
  set(x:s) : ()
}

val state = handler {
  return x -> fun(s) : cps _ { x }     // needs an annotation to bring it in the cps effect
  get()    -> fun(s) { resume(s)(s) }
  set(t)   -> fun(s) { resume(())(t) }
}

fun test() {
  set(2)
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  uncps{ state(test)(0).println }
}