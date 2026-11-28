// Calling a function selected from a field twice: the first call may yield,
// so the monadic translation must check for a yield before the second call.
effect yld
  ctl yld() : ()

value struct tester( sync : () -> <yld,console> () )

fun settle( t : tester ) : <yld,console> ()
  (t.sync)()
  (t.sync)()

pub fun main()
  var n := 0
  with ctl yld()
    n := n + 1
    println("yield " ++ n.show)
    resume(())
  settle(Tester(fn() { println("sync"); yld() }))
  println("done")
