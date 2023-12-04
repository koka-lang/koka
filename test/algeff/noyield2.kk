effect fun bar() : int
effect fun foo(x:(),y:(),z:()) : int

fun main() 
  with fun bar() 1
  with fun foo(x,y,_z)
          //trace("hi") 
          bar()
  val x = foo((),(),())
  x.println
