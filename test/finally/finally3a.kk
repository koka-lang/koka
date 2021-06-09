// Test finally
effect fun inc() : int
effect except raise( msg : string ) : a

fun bar() {
  println("action: " ++ inc().show )
  raise("ouch")
  42
}

fun foo()
{
  var i : int := 42
  with fun inc() { val j = i; i := i + 1; j }
  with return(x) { println("return, local: " ++ i.show ); x }
  with finally   { println("finally, local: " ++ i.show) }
  bar()
}

fun main() {
  with except raise(msg) { println("raised: " ++ msg) }
  with return(x)  { println( "result: " ++ (x:int).show ) }
  with finally    { println("exc finalize") }
  foo()
}
