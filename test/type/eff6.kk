function remote( f : () -> exn () ) : exn () { () }

function test(f) { print("hi"); remote(f); f() }