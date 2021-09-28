// Test finally
effect state {
  ctl inc() : int
}

effect exc {
  ctl raise( s : string ) : a
}

fun bar() {
  println("action: " ++ inc().show )
  raise("ouch")
  42
}

fun foo() : _ int
{
  var i := 41
  handle(bar) {
    return (x){
      println("return, local: " ++ i.show )
      x
    }
    finally {
      println("finally, local: " ++ i.show)
    }
    ctl inc(){ 43 }   // no resume.. should invoke finalizers
  }
}

fun main() {
  handle(foo) {
    return (x){ println( "result: " ++ (x:int).show ) }
    ctl raise(msg){ println("raised: " ++ msg) }
    finally { println("exc finalize") }
  }
}
