import std/num/int32

effect st 
  control get() : int32
  control set(i : int32)  : ()


fun state( init : int32, action : () -> <st|e> a ) : e a 
  with return(f){ f(init) }
  with handler 
    return(x)      { fn(s){ x } }
    rcontrol get() { fn(s){ rcontext.resume(s)(s) } }
    rcontrol set(s){ fn(_){ rcontext.resume(())(s) } }    
  action()

fun counter( c : int32 ) : _ int 
  val i = get()
  if i==zero then c.int else 
    set(i.dec)
    counter(c.inc)

fun main() 
  with state(10100100.int32)  // since v2.2.0: 1010 is ok, but higher runs out of stack with -O2 :-(
  counter(zero).println
