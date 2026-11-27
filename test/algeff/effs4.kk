// test state as fun result from a handler (where resume is captured)

effect state<s> {
  ctl get()    : s;
  ctl set(x:s) : ()
}

val state = handler {
  return(x){ fn(s){ x } }
  raw ctl get() { fn(s){ rcontext.resume(s)(s) } }
  raw ctl set(t){ fn(s){ rcontext.resume(())(t) } }
}

fun test() {
  set(2)
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  state(test)(0).println 
}