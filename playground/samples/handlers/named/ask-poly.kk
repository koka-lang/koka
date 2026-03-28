/* Shows the use of a named effect handlers for multiple reader effects.

   Prerequisites:
   - learn/var
   - learn/with
   - handlers/basic
*/
module ask-poly

named effect reader<a>     // named handler for files
  fun ask() : a            // `:(ev<reader<a>>)   -> <exn> a`


// A reader handler
fun reader( x, action )
  with r <- named handler
              fun ask() x
  action(r)


// Using two readers.
// This particular example is of course silly (one can just pass regular parameters)
// but it can be useful if the named effect performs operations itself
// in its own context (like reading from the current http connection)
pub fun read2files(r1, r2 : ev<reader<a>>, ?show : a -> string)
  println(r1.ask() ++ " " ++ r2.ask().show)


pub fun main()
  with r1 <- reader("hello")
  with r2 <- reader(42)
  read2files(r1,r2)
