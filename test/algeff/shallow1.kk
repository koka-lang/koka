// use "raw ctl" (`rcontrol`) to get a 
// resume context as `rcontext`. 
// We can `resume`, `resume-shallow`, or `finalize` such context.
module shallow1

effect consume<a> 
  ctl await() : a


effect produce<a> 
  ctl yield( value : a) : ()


// fun pipe( produce, consume ) 
fun pipe( produce : () -> <div,produce<a>|e> b, consume : () -> <consume<a>,div|e> b) : <div|e> b
  with raw ctl await()  
    copipe( fn(x) rcontext.resume-shallow(x), produce )  
  consume()

// fun copipe( consume, produce )
fun copipe( consume : a -> <consume<b>,div|e> c, produce : () -> <div,produce<a>|e> c) : <div|e> c
  with raw ctl yield(x)
    pipe( fn() rcontext.resume-shallow(()), fn() consume(x) )
  produce()


fun main() 
  pipe {
    yield(20)
    yield(22)
    yield(30)
  } { 
    val x : int = await()
    val y = await()
    println( x+y )
  }

