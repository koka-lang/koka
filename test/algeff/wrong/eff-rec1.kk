effect cow {
  control moo() : (() -> cow ())
}

val cow = handler {
  control moo() {
    println("moo")
    resume( {moo()() } )
  }
}

fun main() {
  cow {
    moo()()
  }
}
