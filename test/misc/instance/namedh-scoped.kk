abstract type ix<s::S> { Ix(index :int) }

effect vec<s::S> {
  fun push( s : string ) : ix<s>
  fun lookup( ix : ix<s> ) : string
}

fun vec(action : forall<s> () -> <vec<s>|e> a ) : e a {  // rank-2 signature required
  var xs : list<string> := []
  with {
    fun push( s )  { val i = xs.length; xs := xs + [s]; Ix(i) }
    fun lookup(ix) { xs[ix.index].default("") }  // index is always valid!
  }
  action()
}

fun main() {
  with vec
  val ix = push("hello")
  println( lookup(ix) ++ " world" )
}

/* // rejected by the type checker as the index escapes the scope of vec
fun escape() {
  with vec
  push("hello")  
}

fun wrong() {
  with vec
  val ix = escape()
  println( lookup(ix) ++ " world")
}
*/
