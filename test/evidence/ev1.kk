public module ev1

import core/hnd

effect exc {
  control throw() : a
}


effect amb {
  fun flip() : bool
}

effect state<a> {
  fun get()    : a
  fun set(x:a) : ()
}

ambient val width : int

effect instance file {
  fun read() : string
}
