effect cow {
  ctl moo() : (() -> cow ())
}

val cow = handler {
  ctl moo() {
    println("moo")
    resume( {moo()() } )
  }
}

fun main() {
  cow {
    moo()()
  }
}
