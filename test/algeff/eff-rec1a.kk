ambient rec fun moo() : (() -> <moo,div> ())

fun main() {
  with fun moo() {
    println("moo")
    ({ moo()() })
  }
  moo()()
}
