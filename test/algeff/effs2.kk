effect amb {
  fun flip() : bool
}

effect state<s> {
  fun get()  : s;
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

fun state( init, action ) {
  var s := init
  handle({mask<local>(action)}) {
    fun get()  { s }
    fun set(i) { s := i }
  }
}

fun amb(action) {
  handle(action) {
    return x -> [x]
    flip()   -> append( resume(False), resume(True) )
  }
}


fun test1() {
  amb(xor)
}


fun test2()  {
  state(0){ amb(foo) }
}

fun test3()  {
  amb{ state(0,foo) }
}

fun main() {
  test1().show.println
  test2().show.println
  test3().show.println
}
