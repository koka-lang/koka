effect tick {
  fun tick() : int
}

fun lticker( action ) { // : () -> <tick|e> a ) : <local<h>|e> a {
  use s = local(0)
  handle(action) {
    fun tick() {
      s := s + 1
      resume(s)
    }
  }
}

fun ticker( action : () -> <tick|e> a ) : e a {
  var s := 0
  handle(inject-st(action)) {
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
