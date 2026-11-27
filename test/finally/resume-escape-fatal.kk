// Calling a `resume` after its `ctl` clause returned without resuming is a fatal error.
// When the clause returns without resuming, the resumption is finalized: the `finally` clauses of the continuation run.
// Resuming it afterwards would run the continuation again after its `finally` already ran, so it aborts with "trying to resume a finalized resumption" instead.
// (A clause whose `resume` must outlive it is a `raw ctl`, which finalizes explicitly.)
// The fatal error goes to stderr, which the test does not compare, so the expected output ends at "resuming the stored resume".
effect ask
  ctl ask() : int

fun main()
  var saved := Nothing
  val r =
    with ctl ask()
      saved := Just(resume)   // the resume escapes the clause
      0                       // and the clause returns without resuming
    with finally
      println("finally")      // printed once, when the clause returns
    val x = ask()
    println("continued after ask")  // never printed: the continuation must not run after its finally
    x + 1
  println("r = " ++ r.show)
  match saved
    Just(k) ->
      println("resuming the stored resume")
      val y = k(41)                                  // fatal error: the resumption was finalized
      println("not reached: k(41) = " ++ y.show)    // never printed
    Nothing -> ()
