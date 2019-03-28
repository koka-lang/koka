// File handling with initially and finally

ambient control flip() : bool

// files are named handlers (and thus scoped)
ambient instance file {
  fun write(s : string) : ()
}

fun message() : <flip> string {
  if (flip()) then "hi" else "bye"
}

fun foo() {
  var h := 0                               // file handle
  with f = instance {                      // file as a named handler instance
    initially     { h := fopen("foo") }
    finally       { fclose(h); h := 0 }
    fun write(s)  { fwrite(h,s) }
  }
  f.write(message())  // note: `f` stays stable unlike `h` which gets re-initialized
}

fun main() {
  with return(x) { () }
  with control flip() { resume(False) + (resume(True) : list<()>) }
  [foo()]
}


// Low level files

alias hfile = int

fun fopen( fname : string ) : <console,ndet> hfile {
  val f = unique() + 1
  println("open : " + f.show)
  f
}

fun fclose( f : hfile ) {
  println("close: " + f.show)
}

fun fwrite( f : hfile, msg ) {
  println("write: " + f.show + ": " + msg )
}
