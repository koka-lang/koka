public module ev1

import std/core/hnd

effect exc {
  control throw() : a
}


effect amb {
  fun flip() : bool
}

effect state<a> {
  fun get()    : a
  fun set(x:a) : ()
  fun throw(s:string,x:a) : b
}

ambient val width : int


effect instance file<a> {
  fun read(x:a) : string
  fun fthrow(x:a) : b
}

effect rec cow {
  control moo() : (() -> cow ())
}
