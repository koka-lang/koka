// test name-clashes

effect state<s> {
  fun get()    : s
  fun set(x:s) : ()
}


fun state( init, action ) {
  var s : int := init   
  with {
    fun get()  { s }
    fun set(i) { s := i }
  }
  action()
}

fun test() {
  if (get() > 0) then "positive" else "negative or zero"
}

fun main() {
  state(0,test).println
}