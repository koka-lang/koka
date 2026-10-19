// `firstof` (and `timeout`) should return (or raise) the result of the first strand
// that finishes and cancel the others; an exception should not be lost.
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
  test("timeout completes") { timeout(1.seconds){ wait(10.milli-seconds); 42 }.show }
  test("timeout expires")   { timeout(10.milli-seconds){ wait(1.seconds); 42 }.show }
  test("timeout raises")    { timeout(1.seconds){ wait(10.milli-seconds); throw("boom"); 42 }.show }
  test("firstof raises")    { firstof({ wait(1.seconds); "left" }, { wait(10.milli-seconds); throw("right") }) }
  test("firstof completes") { firstof({ wait(10.milli-seconds); "left" }, { wait(1.seconds); throw("right") }) }
  test("firstof all raise") { firstof({ wait(10.milli-seconds); throw("left") }, { wait(20.milli-seconds); throw("right") }) }
  // the exception is raised first, even though the receive of the other strand already succeeded
  val ch = channel()
  test("firstof raises before completion") { firstof({ ch.receive(); "left" }, { ch.emit(1); throw("right") }) }
