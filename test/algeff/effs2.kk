effect amb {
  control flip() : bool
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
  var s : int := init   
  with {
    return(x)  { (x,s) }
    fun get()  { s }
    fun set(i) { s := i }
  }
  action()
}

val amb = handler {
  return(x)      { [x] }
  control flip() { resume(False) ++ resume(True) }
}

fun test1() {
  amb(xor)
}

fun test2()  {
  state(0){ amb(foo) }.fst
}

fun test3()  {
  amb{ state(0,foo) }.map(fst)
}

fun main() {
  test1().show.println
  test2().show.println
  test3().show.println
}
