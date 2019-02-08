effect tick {
  fun tick() : int
}

fun ticker( action : () -> <tick|e> a ) : e a {
  var s := 0
  handle(action) {
    fun tick() {
      s := s + 1
      resume(s)
    }
  }
}


fun test() {
  ticker {
    print( tick() + tick() )
  }
}
