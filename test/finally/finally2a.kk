// Test finally
effect state {
  fun inc() : int
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
    fun inc(){ i := i+1; i }
  }
}

fun main() {
  handle(foo) {
    return (x){ println( "result: " ++ (x:int).show ) }
    except raise(msg){ println("raised: " ++ msg) }
    finally { println("exc finalize") }
  }
}
