// test name-clashes

effect state<s> {
  get()    : s;
  set(x:s) : ()
}

val state = handler(s) {
  return x -> x
  get()    -> resume(s,s)
  set(t)   -> resume(t,())
}

function test() {
  if (get() > 0) then "positive" else "negative or zero"
}

function main() {
  state(0,test).println
}