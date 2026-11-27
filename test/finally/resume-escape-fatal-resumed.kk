// Calling a `resume` after its `ctl` clause returned is a fatal error, even when the clause resumed before returning.
// The `resume` of a `ctl` clause is valid only while the clause runs; a clause whose `resume` must outlive it is a `raw ctl`.
// (`resume-escape-fatal.kk` tests the same for a clause that returns without resuming.)
// The fatal error goes to stderr, which the test does not compare, so the expected output ends at "resuming the stored resume".
effect ask
  ctl ask() : int

fun main()
  var saved := Nothing
  val r =
    with ctl ask()
      saved := Just(resume)   // the resume escapes the clause
      resume(1)               // and the clause resumes once before returning
    with finally
      println("finally")      // printed once, when the resumed continuation finishes
    val x = ask()
    println("resumed with " ++ x.show)  // printed once, for resume(1)
    x
  println("r = " ++ r.show)
  match saved
    Just(k) ->
      println("resuming the stored resume")
      val y = k(41)                                  // fatal error: the clause has returned
      println("not reached: k(41) = " ++ y.show)    // never printed
    Nothing -> ()
