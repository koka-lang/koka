// test state as fun result from a handler (where resume is captured)

effect state<s> {
  ctl get()    : s;
  ctl set(x:s) : ()
}

val state = handler {
  return(x){ fn(s){ x } }
  ctl get() { fn(s){ resume(s)(s) } }
  ctl set(t){ fn(s){ resume(())(t) } }
}

fun test() {
  set(2)
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  state(test)(0).println 
}