/* Shows the use of a named effect handlers for multiple reader effects.

   Prerequisites:
   - learn/var
   - learn/with
   - handlers/basic
*/
module ask

named effect reader     // named handler for files
  fun ask() : string    // `:(ev<reader>)   -> <exn> string`


// A reader handler
fun reader( msg, action )
  with r <- named handler
              fun ask() msg
  action(r)


// Using two readers.
// This particular example is of course silly (one can just pass regular parameters)
// but it can be useful if the named effect performs operations itself
// in its own context (like reading from the current http connection)
pub fun read2files(r1,r2)
  println(r1.ask() ++ " " ++ r2.ask())


pub fun main()
  with r1 <- reader("hello")
  with r2 <- reader("world")
  read2files(r1,r2)
