import std/num/int32

linear effect st 
  fun get() : int32
  fun set(i : int32)  : ()


fun state( i : int32, action : () -> <st|e> a ) : e a 
  var s := i
  with handler
    fun get() { s }
    fun set(x){ s := x }    
  action()


fun counter( c : int32 ) : <st,div> int 
  val i = get()
  if i==zero then c.int else {
    set(i.dec)
    counter(c.inc)

fun main() 
  with state(100100100.int32)
  counter(zero).println
