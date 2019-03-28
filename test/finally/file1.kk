// File handling with initially and finally
ambient control flip() : bool

// files are named handlers (and thus scoped)
ambient instance file {
  fun write(s : string) : ()
}

fun message() : <flip> string {
  if (flip()) then "bye" else "hi"
}

fun foo() {
  with f = open-file("foo")
  with g = open-file("bar")
  f.write(message())
  g.write(message())
}

fun open-file(fname,action) {
  var h := 0                                      // low-level file handle
  with initially { h := fopen(fname) }
  with finally   { fclose(h); h := 0 }
  with f = instance fun write(s) { fwrite(h,s) }  // files as a named instance
  action(f)  // note: name `f` stays stable unlike `h` which gets re-initialized
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
