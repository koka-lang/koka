// test state as fun result from a handler (where resume is captured)

effect state<s> {
  control get()    : s;
  control set(x:s) : ()
}

val state = handler {
  return(x){ fn(s){ x } }
  control get() { fn(s){ resume(s)(s) } }
  control set(t){ fn(s){ resume(())(t) } }
}

fun test() {
  set(2)
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  state(test)(0).println 
}