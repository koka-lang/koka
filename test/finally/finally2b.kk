// Test finally
effect state {
  control inc() : int
}

effect exc {
  except raise( s : string ) : a
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
    control inc(){ 43 }   // no resume.. should invoke finalizers
  }
}

fun main() {
  handle(foo) {
    return (x){ println( "result: " ++ (x:int).show ) }
    except raise(msg){ println("raised: " ++ msg) }
    finally { println("exc finalize") }
  }
}
