fun remote( f : () -> exn () ) : exn () = ()

fun test(f) { remote(f); f();  }