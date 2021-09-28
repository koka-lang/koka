effect amb {
  ctl flip() : bool
}

effect state<s> {
  fun get()  : s
  fun set(i:s) : ()
}

fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}


fun foo() : <amb,state<int>> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}         

 
// handlers

val amb_handle = handler {
  return x -> [x]
  ctl flip() -> resume(False) ++ resume(True)
}

fun state_handle(init,action) {
  var i := init
  with {
    fun get() { i }
    fun set(j){ i := j }
  }
  action ()
}

fun test1() {
  amb_handle(xor)
}


fun test2()  {
  state_handle(0){ amb_handle(foo) }
}

fun test3()  {
  amb_handle{ state_handle(0,foo) }
}

fun main() {
  test1().show.println
  test2().show.println
  test3().show.println
}
