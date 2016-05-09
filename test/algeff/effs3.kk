// test name-clashes

effect state<s> {
  get() : s;
  set(x:s) : s
}

val state = handler(s) {
  return x -> x
  get()    -> resume(s,s)
  set(s)   -> resume((),s)
}

function main() {

}