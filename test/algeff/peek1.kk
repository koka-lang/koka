module peek1

effect<a> val peek : a             // get the state
effect<a> ctl poke( x : a ) : ()   // set the state to x

fun ppstate( init : a, action : () -> <peek<a>,poke<a>|e> b ) : e b
  with val peek = init
  with ctl poke(x)
    mask<peek>
      with val peek = x
      resume(())
  action()

fun main()
  val res = ppstate(1)
              val x : int = peek
              poke(41)
              x + peek
  res.println
