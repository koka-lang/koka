effect cow {
  fun moo() : (() -> cow ())
}

val cow = handler {
    moo() -> {
        println("moo")
        resume( {moo()() } )
    }
}

fun main() {
    cow {
        moo()()
    }
}
