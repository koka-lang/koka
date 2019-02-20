//fun try( action : () -> <exn|e> a ) : e try<a>
//fun untry( x : try<a> ) : <exn|e> a

fun foo( action ) {
  val x = try(action)
  println("tried: exn: " + x.maybe.bool.show )
  x.untry
}


fun foo1( action ) {
  val x = try{ inject<exn>(action) }
  println("tried1: exn: " + x.maybe.bool.show )
  x.untry
}

fun inject-tail-exn( action ) { // : () -> <exn|e> a ) : <exn,exn|e> a {
  inject behind<exn>(action)
}

fun foo2( action ) {
  val x = try{ inject-tail-exn(action) }
  println("tried2: exn: " + x.maybe.bool.show )
  x.untry
}

fun bar(f,action) {
  try {
    try {
      f(action)
    }
    fun(exn) {
      println("caught on inner level: " + exn.message)
    }
  }
  fun(exn) {
    println("caught on outer level: " + exn.message)
  }
}

fun test(f) {
  bar(f) {
    //error("outer")
    inject<exn>{error("injected")}
    error("outer")
  }
  println("")
}

fun main() {
  test(foo)   // unifies: should catch on inner level
  test(foo2)  // abstracts: should catch on outer level
  test(foo1)  // identity: should catch on outer level
}
