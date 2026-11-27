fun io-action(): io ()
  ()

fun main(action: () -> <io-noexn|e> ()): <io|e> int
  var x := 1
  io-action()
  action()
  x
