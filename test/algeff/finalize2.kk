effect ctl yield(s: string): ()

fun main()
  with ctl yield(s)
    println(s)
    resume(())
  with finally
    println("Pre final")
    yield("Final")
    println("Post final")
  yield("Error")
  2