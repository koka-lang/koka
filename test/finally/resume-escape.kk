// A `resume` that escapes its `ctl` clause, in the cases that are allowed.
// The `resume` of a `ctl` clause is valid while the clause runs, so it may be passed to other functions and called (even more than once) before the clause returns.
// When the clause returns without having resumed, the resumption is finalized: its `finally` clauses run.
// Calling an escaped `resume` after the clause returned is a fatal error, tested in `resume-escape-fatal.kk` and `resume-escape-fatal-resumed.kk`.
effect ask
  ctl ask() : int

fun twice( k : int -> e int ) : e int
  k(1) + k(2)

// The clause hands `resume` to `twice`, which calls it twice before the clause returns.
// Each call runs the continuation to the end, with its own `finally`.
fun resumed-by-helper()
  with ctl ask()
    twice(resume)
  with finally
    println("finally")      // printed once for each run of the continuation
  val x = ask()
  println("resumed with " ++ x.show)
  x

// The clause stores `resume` and returns without resuming.
// The resumption is finalized once (printing "finally"), and the stored `resume` is never called.
fun never-resumed()
  var saved := Nothing
  val r =
    with ctl ask()
      saved := Just(resume)
      0
    with finally
      println("finally")      // printed exactly once, when the clause returns
    ask() + 1
  if saved.is-just then println("saved")
  r

effect flip
  ctl flip() : bool

// The clause yields to `flip`, whose handler resumes it twice, so there are two copies of the clause.
// Each copy has its own state: the first copy resumes, and the second returns without resuming, which finalizes the resumption (printing "finally" a second time).
fun copies() : <console,div> int
  with ctl flip() int/(+)(resume(True), resume(False))
  with ctl ask()
    if flip() then resume(1) else 0
  with finally
    println("finally")      // printed by the first copy's run of the continuation, and by the second copy's finalization
  val x = ask()
  println("resumed with " ++ x.show)
  x

fun main()
  resumed-by-helper().println
  never-resumed().println
  copies().println
