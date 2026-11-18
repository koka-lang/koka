// `firstof` (and `timeout`) is decided by the first strand to finish, with its
// value or its exception, and cancels the others.
import std/async
import std/time/duration
import std/time/timer

fun test( name : string, action : () -> <async,ndet,exn> string ) : <async,ndet,console> ()
  val (t,res) = elapsed{ try(action) }
  val msg = match res
    Ok(s)      -> s
    Error(exn) -> "exception " ++ exn.message
  println(name ++ ": " ++ msg ++ (if t < 500.milli-seconds then "" else " (too slow)"))

fun main()
  test("timeout completes")      { timeout(1.seconds){ wait(10.milli-seconds); "done" }.default("timed out") }
  test("timeout expires")        { timeout(10.milli-seconds){ wait(1.seconds); "done" }.default("timed out") }
  test("timeout raises at once") { timeout(1.seconds){ throw("early"); "done" }.default("timed out") }
  test("timeout raises later")   { timeout(1.seconds){ wait(10.milli-seconds); throw("late"); "done" }.default("timed out") }
  test("firstof, one raises")    { firstof({ wait(1.seconds); "slow" }, { wait(10.milli-seconds); throw("fast") }) }
  test("firstof, both raise")    { firstof({ wait(10.milli-seconds); throw("first") }, { wait(20.milli-seconds); throw("second") }) }
  // the right strand finishes first; the left one completes too, as its resumption
  // was already queued, but does not decide
  val ch = channel()
  test("firstof, both complete") { firstof({ ch.receive(); "left" }, { ch.emit(1); "right" }) }
  // when an outer timeout cancels every strand, `firstof` is canceled too
  test("firstof, all canceled")  { timeout(10.milli-seconds){ firstof({ wait(1.seconds); "a" }, { wait(1.seconds); "b" }) }.default("canceled") }
  // `interleaved` still needs every strand: an exception cancels the others
  test("interleaved raises")     { val (a,b) = interleaved({ wait(1.seconds); "a" }, { wait(10.milli-seconds); throw("b"); "b" }); a ++ b }
