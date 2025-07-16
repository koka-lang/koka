module peek2

effect<a> ctl peek() : a             // get the state
effect<a> ctl poke( x : a ) : ()   // set the state to x

fun ppstate( init : a, action : () -> <peek<a>,poke<a>|e> b ) : e b
  with ctl peek()  resume(init)
  with ctl poke(x)
    mask<peek<_>>
      with ctl peek() resume(x)
      resume(())
  action()

fun main()
  val res = ppstate(1)
              val x : int = peek()
              poke(41)
              x + peek()
  res.println
