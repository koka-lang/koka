
effect state<s> {
  ctl get() : s
  ctl put(x : s) : ()
}

fun state(init,action) {
  var s := init
  with {
    fun get(){ s }
    fun put(t){ s := t }
  }
  action()
}

// we cannot create landin's knot with the state effect!
fun landin() {
  fun f()
     val g = get()
     g()
  put(f)
  f()
  42
}

fun main() {
  state(fn() (),landin).println  // 42
}