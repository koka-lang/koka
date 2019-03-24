// Test a scoped version of ambiguity to see if effect
// inference will correctly generate the right code:
// 1. generate binds in the branches
// 2. or, infer a `normal` branch such that scoped optimization is not used
public module effs1b

effect amb {
  fun flip() : bool
}
 
fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

fun amb( action ) {
   handle(action) {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

fun main() {
  amb(xor).show.println
}

