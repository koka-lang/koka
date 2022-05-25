effect choose
  ctl flip() : bool

fun mystery() : <choose, console> bool 
  val b = flip()
  println("b = " ++ b.show)
  b

// is `mystery` ever true?
fun satisfiable-no-short-circuit() : <console> bool 
  with ctl flip()
    // for each input flip(), try both values
    ( (resume(True) || resume(False)) : bool )
  mystery()

fun satisfiable-short-circuit() : <console> bool 
  with ctl flip()
    (resume(True) : bool) || resume(False)
  mystery()

fun main() 
  satisfiable-no-short-circuit().println
  satisfiable-short-circuit().println
