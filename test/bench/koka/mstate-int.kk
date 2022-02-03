effect st 
  ctl get() : int
  ctl set(i : int)  : ()


fun state( init : int, action : () -> <st|e> a ) : e a 
  with return(f) f(init)
  handle action
    return(x)      (fn(s) x)
    raw ctl get()  (fn(s) rcontext.resume(s)(s))
    raw ctl set(s) (fn(_) rcontext.resume(())(s))    

fun counter( c : int ) : _ int 
  val i = get()
  if (i==0) then c else 
    set(i.dec)
    counter(c.inc)
  

pub fun main() 
  with state(10100)
  counter(0).println
