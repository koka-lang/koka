fun remote( f : () -> exn () ) : exn () = ()

fun test(f) { print("hi"); remote(f); f() }