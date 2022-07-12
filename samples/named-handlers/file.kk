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

named effect file
  fun read-line() : string   // `:(file)   -> <exn> a`


// a (named) handler instance for files
fun file(fname,action) 
  var content := read-text-file(fname.path).lines
  with f <- named handler 
    fun read-line() 
      match content  
        Nil -> "" 
        Cons(x,xx) -> { content := xx; x }
  action(f)

fun cat-lines( t : (file,file) ) 
  t.fst.read-line() ++ "\n" ++ t.snd.read-line()


pub fun main() 
  with f1 <- file("artifact1.txt")
  with f2 <- file("artifact2.txt")
  cat-lines( (f1,f2) ).println

/*
// this is illegal and will throw an exception (as `f` escapes its scope)
fun wrong-escape1() 
  with f <- file("artifact1.txt")
  f

pub fun test() 
  val f = wrong-escape1()
  f.read-line.println
*/