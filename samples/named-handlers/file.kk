/* Shows the use of a named effect handlers to model files.

   Prerequisites:
   - syntax/var
   - syntax/with
   - handlers/basic
   - named-handlers/basic
*/
module file
import std/os/path
import std/os/file

named effect file {
  fun read-line() : string   // `:(file)   -> <exn> a`
}


// a (named) handler instance for files
fun file(fname,action) {
  var content := read-text-file(fname.path).lines
  with f = named handler {
    fun read-line() {
      match(content) {
        Nil        { "" }
        Cons(x,xx) { content := xx; x }
      }
    }
  }
  action(f)
}

public fun main() {
  with f1 = file("packagex.yaml")
  with f2 = file("stackx.yaml")
  println( f1.read-line() ++ "\n" ++ f2.read-line() )
}
