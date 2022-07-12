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
  fun read-line() : string   // `: (f : file<s>) -> scope<s> string`


// a handler instance for files
fun file(fname : string, action : forall<s> file<s> -> <scope<s>|e> a ) : e a 
  var i := 0
  with f <- named handler 
    fun read-line()
      i := i + 1
      (fname ++ ": line " ++ i.show)
  action(f)


fun read-both( f1 : file<s1>, f2 : file<s2> ) : <console,scope<s1>,scope<s2>|e> () 
  println( f1.read-line() ++ "\n" ++ f2.read-line() )


pub fun main() 
  with f1 <- file("artifact1.txt")
  with f2 <- file("artifact2.txt")
  read-both(f1,f2)
  //println( f1.read-line() ++ "\n" ++ f2.read-line() )
  //println( f2.read-line() ++ "\n" ++ f1.read-line() )
  //wrong-escape1().read-line().println
  //wrong-escape2()()
  
/*
// escapes the scope
fun wrong-escape1() 
  with f = file("artifact1.txt")
  f
*/

/*
// escapes the scope via a capture
fun wrong-escape2() 
  with f <- file("artifact1.txt")
  fn() f.read-line()
*/


