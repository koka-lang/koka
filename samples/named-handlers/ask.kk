/* Shows the use of a named effect handlers for multiple reader effects.

   Prerequisites:
   - syntax/var
   - syntax/with
   - handlers/basic
*/
module ask

named effect reader     // a named handler for an environment
  fun ask() : string    // `:(reader) -> <exn> string`


// A reader handler
fun reader( msg, action ) 
  with r <- named handler 
              fun ask() msg   // always resume with `msg`
  action(r)


// Using two readers.
// This particular example is of course silly (one can just pass regular parameters)
// but it can be useful if the named effect performs operations itself
// in its own context (for example, like reading from the current http connection)
pub fun test(r1,r2) 
  r1.ask() ++ " " ++ r2.ask()


pub fun main() 
  with r1 <- reader("hello")
  with r2 <- reader("world")
  test(r1,r2).println

