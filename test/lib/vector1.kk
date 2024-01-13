// Test by Anton Lorenzen, issue #416
effect choice
  ctl flip() : bool

fun test-vector()
  with handler
    ctl flip()
      val v1 = resume(True)
      val v2 = resume(False)
      v1.append(v2)
  with return(v) [v]
  vector-init(5, fn(i) flip())

fun test-list() // Just like test-vector but for lists
  with handler
    ctl flip()
      val v1 = resume(True)
      val v2 = resume(False)
      v1.append(v2)
  with return(v) [v]
  list(0, 4, fn(i) flip())

fun main()
  val vs = test-vector()
  // val vs = test-list()
  println(vs.length())
  vs.foreach(fn(v) print(v.length()))

