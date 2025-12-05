effect ping 
  fun ping(): ()

fun hand(f, action)
  var v := ()
  with fun ping() 
    val x = f(1)
    ()
  action()
