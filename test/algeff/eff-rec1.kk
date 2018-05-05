effect rec cow {
  fun moo() : (() -> <cow,div>  ())
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
