effect amb {
  fun flip() : bool
}

effect state<s> {
  fun get()  : s
  fun set(i:s) : ()
}

fun xor() : <amb,exn> bool {
  val p = flip()
  if (p) error("oops")
  val q = flip()
  (p||q) && not(p&&q)
}


fun foo() : <amb,state<int>,io> bool {
  val p = flip()
  try {
    finally {
      val i = get()
      set(i+1)
      if (i>0 && p) then xor() else False
    }
    { println("exit") }
  }
  fun(exn) { False }
}


// handlers

val amb_handle = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

val state_handle = handler(i) {
  return x -> x
  get()    -> resume(i,i)
  set(j)   -> resume((),j)
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
  //test1().show.println
  test2().show.println
  test3().show.println
}
