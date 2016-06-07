function remote( f : () -> exn () ) : exn () { () }

function test(f) { remote(f); f();  }