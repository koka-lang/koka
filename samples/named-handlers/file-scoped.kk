/* Shows the use of a named effect handlers to model files.

   Prerequisites:
   - syntax/var
   - syntax/with
   - handlers/basic
   - named-handlers/basic
*/
module file-scoped
import std/os/path
import std/os/file

named scoped effect file<s::S> {
  fun read-line() : string   // `: (file) -> a`
}


// a handler instance for files
fun file(fname : string, action : forall<s> file<s> -> e a ) : e a {
  var i := 0
  with f = named handler {
    fun read-line() {
      i := i + 1
      (fname + ": line " + i.show)
    }
  }
  action(f)
}


fun read-both( f1 : file<s1>, f2 : file<s2> ) : <console,scope<s1>,scope<s2>> () {
  println( f1.read-line() + "\n" + f2.read-line() )
}

public fun main() {
  with f1 = file("package.yaml")
  with f2 = file("stack.yaml")
  //println( f1.read-line() + "\n" + f2.read-line() )
  read-both(f1,f2)
  //wrong-escape()()
}


/*
fun wrong-escape2() {
  with f = file("stack.yaml")
  fn(){ f.read-line() }
}

fun wrong-escape1() {
  with f = file("stack.yaml")
  f
}
*/
