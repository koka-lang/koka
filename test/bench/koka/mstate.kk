import std/num/int32

effect st 
  ctl get() : int32
  ctl set(i : int32)  : ()

fun state( init : int32, action : () -> <st|e> a ) : e a 
  with return(f) f(init) 
  handle action
    return(x)      (fn(s) x)
    raw ctl get()  (fn(s) rcontext.resume(s)(s))
    raw ctl set(s) (fn(_) rcontext.resume(())(s))   

fun counter( c : int32 ) : _ int 
  val i = get()
  if i==zero then c.int else 
    set(i.dec)
    counter(c.inc)

pub fun main() 
  with state(10100100.int32)  // since v2.2.0: 1010 is ok, but higher runs out of stack with -O2 :-(
  counter(zero).println
