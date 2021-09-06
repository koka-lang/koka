/* Shows the use of a named effect handlers for multiple reader effects.

   Prerequisites:
   - syntax/var
   - syntax/with
   - handlers/basic
*/
module ask

named effect reader     // named handler for files
  fun ask() : string    // `:(reader)   -> <exn> string`


// A reader handler
fun reader( msg, action ) 
  with r = named handler 
             fun ask(){ msg }  
  action(r)


// Using two readers.
// This particular example is of course silly (one can just pass regular parameters)
// but it can be useful if the named effect performs operations itself
// in its own context (like reading from the current http connection)
public fun test(r1,r2) 
  println(r1.ask() ++ " " ++ r2.ask())


public fun main() 
  with r1 = reader("hello")
  with r2 = reader("world")
  test(r1,r2)

