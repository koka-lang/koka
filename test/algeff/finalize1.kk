// test finalizers

effect exc {
  fun fail( msg : string ) : a
}

val exc1 = handler {
  fail(msg) -> finalize(msg)
}

val exc2 = handler {
  fail(msg) -> msg
}

fun test() {
  finally {
    fail("ouch")
    "ok"
  }
  {
    println("finalizing..")  
  }
}

fun main() {
  exc1(test).println
  exc2(test).println
}