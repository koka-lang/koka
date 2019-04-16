ambient fun tick() : int


fun lticker( action ) { // : () -> <tick|e> a ) : <local<h>|e> a {
  var s := 0
  with fun tick() { s := s + 1; s } in action()
  return ({ s := s + 1; s})
}

fun ticker( action : () -> <tick|e> a ) : e a {
  var s := 0
  with fun tick() {
      s := s + 1
      s
    }
  mask<tick>(action)
}

fun test() {
  with ticker
  print( tick() + tick() )
}
