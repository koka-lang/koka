effect reader {
  fun read() : int
}

fun foo() : reader int {
  fun bar( y : int ) : reader int {
    return y    
  }
  val x = bar(19)
  x + 1
}
