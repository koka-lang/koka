//fun try( action : () -> <exn|e> a ) : e try<a>
//fun untry( x : try<a> ) : <exn|e> a

fun foo0( action ) {
  val x = try(action)
  println("tried0: exn: " ++ x.maybe.bool.show )
  x.untry
}


fun foo1( action ) {
  val x = try{ mask<exn>(action) }
  println("tried1: exn: " ++ x.maybe.bool.show )
  x.untry
}

fun foo2( action ) {
  val x = try{ mask behind<exn>(action) }   // inject behind the outer exn, i.e. inject the injects in action
  println("tried2: exn: " ++ x.maybe.bool.show )
  x.untry
}


// equivalent to foo2
fun foo3( action ) {
  val x = mask<exn>{ try{ action() } }
  println("tried3: exn: " ++ x.maybe.bool.show )
  x.untry
}

fun bar(f,action) {
  try {
    try {
      f(action)
    } fn(exn) {
      println("caught on inner level: " ++ exn.message)
    }
  } fn(exn) {
    println("caught on outer level: " ++ exn.message)
  }
}

fun test(f,err) {
  bar(f) {
    err()
    mask<exn>{throw("masked")}
    throw("outer2")
  }
  println("")
}

fun main() {
  fun err(){ throw("outer1") }
  fun noerr(){ () }

  test(foo0,noerr)   // unifies: should catch on inner level
  test(foo2,noerr)  // abstracts: should catch on outer level
  test(foo3,noerr)  // abstracts: should catch on outer level
  test(foo1,noerr)  // identity: should catch on outer level

  test(foo0,err)   // unifies: should catch on inner level
  test(foo2,err)  // abstracts: should catch on outer level
  test(foo3,err)  // abstracts: should catch on outer level
  test(foo1,err)  // identity: should catch on outer level
}
