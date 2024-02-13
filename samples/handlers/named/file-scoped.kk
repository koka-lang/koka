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

named scoped effect file<s::S>
  fun read-line() : string   // `: (f : ev<file<s>>) -> file<s> string`

// fun read-line : ev<file<s>> -> file<s> string

// a handler instance for files
fun file(fname : string, action : forall<s> ev<file<s>> -> <file<s>|e> a ) : e a
  var i := 0
  with f <- named handler
    fun read-line()
      i := i + 1
      (fname ++ ": line " ++ i.show)
  action(f)


fun read-both( f1 : ev<file<s1>>, f2 : ev<file<s2>> ) : <console,file<s1>,file<s2>|e> ()
  println( f1.read-line() ++ "\n" ++ f2.read-line() )

pub fun main()
  with f1 <- file("package.yaml")
  with f2 <- file("stack.yaml")
  read-both(f1,f2)
  // println( f1.read-line() ++ "\n" ++ f2.read-line() )
  //wrong-escape2()()


// fun wrong-escape2()
//   with f <- file("stack.yaml")
//   fn() f.read-line()


// fun wrong-escape1()
//   with f = file("stack.yaml")
//   f

