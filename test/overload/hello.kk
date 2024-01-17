// \x -> show (read x)  // : String -> String
// fn(x) show (int/read x)


fun int/eq( x : int, y : int ) : bool
  (x == y)

fun char/eq( x : char, y : char ) : bool
  (x == y)

fun string/eq( x : string, y : string ) : bool
  (x == y)

// Polymorphic list equality
fun list/eq(xs: list<a>, ys: list<a>, ?eq : (a,a) -> bool) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> list/eq(xx,yy)
      _ -> False
    Nil -> ys.is-nil



// A struct with a polymorphic field
struct hello<a>
  a: list<a>
  x: int
  y: char
  z: list<string>

fun equal( x : a, y : a, ?eq : (a,a) -> bool ) : bool
  eq(x,y)

// Argument 1 for allowing bypassing:
//   - infix operators are really problematic, and require large amounts of rewriting expressions to resolve appropriately
//     - Workaround 1: maybe allow qidop in addition to qop in infix expressions - also why is it called qop if it doesn't allow qualified?
//             (this would also be useful for the above list/(==) definition)
//             though I agree that qidops look a bit strange infix, but it is something I could get used to maybe
//     - Workaround 2: allow bypassing for infix expressions / operators
//             (this would be inconsistent)
//     - Workaround 3: Only allow bypassing for non-polymoprhic expressions?
//             (i.e. the arguments h1.x and h2.x are both of type int which is not polymorphic)
//     - The workaround of having a top level function that takes implicits by a different name
//             (i.e. cmp and xcompare) really doesn't work for operators
//
// In other words, ideally this example would work.
fun hello/eq( h1 : hello<a>, h2 : hello<a>, ?eq : (a,a) -> bool ) : bool
  // Also I'm confused why h1.a == h2.a doesn't work currently since it should use the local `(==)`
  eq(h1.a, h2.a) && eq(h1.x,h2.x) && eq(h1.y,h2.y) && eq(h1.z,h2.z)
  // list/eq( h1.a, h2.a )
  // equal(h1.a,h2.a) && equal(h1.x,h2.x) && equal(h1.y,h2.y) && equal(h1.z,h2.z)

// Argument 2 for allowing bypassing:
//  - In the above example it would be confusing for h1.a == h2.a to use the list/(==) type conversion
//    but no other overloaded (==) functions are considered when the types don't match for the other fields

// Argument 3 for allowing bypassing:
//  - One argument Daan made against bypassing is that it would be confusing to have the following example
//      where cmp looks like it might lexically resolve to ?cmp but it isn't
//  - My argument against that was that ?cmp is already a distinguished name by the ? so that it is clear that
//      it is an overloaded identifier, and that parameters should be notified as unused if they are not used
//  - An additional argument in my favor is that already in the current system it is not lexical resolution
//      due to type conversions that do end up using local identifiers
fun hello/cmp( h1 : hello<a>, h2 : hello<a>, ?cmp : (a,a) -> order ) : order
  match int/cmp(h1.x,h2.x)
    Eq -> char/cmp(h1.y,h2.y)
    ord -> ord

// A middle ground might be to resolve as you do without bypassing, then try to bypass
//  if all the arguments are not polymorphic
//  (or maybe only return overloads that don't have implicit parameters) <- probably not (we'd still want h1.z == h2.z to work in the above example)