import std/num/int32

linear effect st 
  fun get() : int32
  fun set(i : int32)  : ()

fun state( i : int32, action : () -> <st|e> a ) : e a 
  var s := i
  handle action
    fun get()  s 
    fun set(x) s := x

fun counter( c : int32 ) : <st,div> int 
  val i = get()
  if i==zero then c.int else
    set(i.dec)
    counter(c.inc)

pub fun main() 
  with state(100_100_100.int32)
  counter(zero).println
