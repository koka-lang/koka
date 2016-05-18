// test locals with initializer

effect state<s> {
  get()    : s;
  set(x:s) : ()
}

val state = handler(s=0) {
  return x -> x
  get()    -> resume(s,s)
  set(t)   -> resume(t,())
}

function fstate(action) {
  handle(action)(s=0) {
    return x -> x
    get()    -> resume(s,s)
    set(t)   -> resume(t,())
  }
}

function test() {
  if (get() > 0) then "positive" else "negative or zero"
}


function main() {
  state(test).println
}