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


pub fun main() 
  with f1 <- file("package.yaml")
  with f2 <- file("stack.yaml")
  println( f1.read-line() ++ "\n" ++ f2.read-line() )

/*
fun wrong-escape1() 
  with f <- file("stack.yaml")
  f

pub fun test() 
  val f = wrong-escape1()
  f.read-line.println
*/