// use "raw control" (`rcontrol`) to get a 
// resume context as `rcontext`. 
// We can `resume`, `resume-shallow`, or `finalize` such context.
public module shallow1

effect consume<a> {
  control await() : a
}

effect produce<a> {
  control yield( value : a) : ()
}

fun pipe( produce, consume ) {
  with rcontrol await() {  
    copipe( fn(x){ rcontext.resume-shallow(x) }, produce )
  }
  consume()
}

fun copipe( consume, produce ) {
  with rcontrol yield(x) {
    pipe( { rcontext.resume-shallow(()) }, { consume(x) } )
  }
  produce()
}

fun main() {
  pipe {
    yield(20)
    yield(22)
    yield(30)
  } { 
    val x : int = await()
    val y = await()
    println( x+y )
  }
}
