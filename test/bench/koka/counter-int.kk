linear effect st 
  fun get() : int
  fun set(i : int)  : ()


fun state( i : int, action : () -> <st|e> a ) : e a 
  var s := i
  with handler
    fun get()  s 
    fun set(x) s := x 
  action()

fun counter( c : int ) : <st,div> int 
  val i = get()
  if i==0 then c else 
    set(i.dec)
    counter(c.inc)

pub fun main() 
  with state(100100100)
  counter(0).println
