// File handling with initially and finally
effect ctl flip() : bool

// files are named handlers (and thus scoped)
named effect file {
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
  var h := 0                             // low-level file handle
  with initially( fn(i){ println("initially: " + i.show + ", h: " + h.show); h := fopen(fname) } )
  with finally { println("finally, h: " + h.show); fclose(h); h := 0 }
  with f = named handler {
    fun write(s) { fwrite(h,s) }         // files as a named instance
  }
  action(f)  // note: name `f` stays stable unlike `h` which gets re-initialized
}

fun main() {
  with return (x){ () }
  with handler {
    return(x)      { [x] }
    ctl flip() { resume(False) + resume(True) }
  }
  foo()
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
