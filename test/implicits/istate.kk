// --------------------------------------------------------------
// Local mutable state defined as implicit control.
// This is a much nicer way to give semantics to state
// versus the state-monad encoding. However, implementations
// need to support frame "smashing" to implement this in constant
// stack space. Paper is forthcoming...
// --------------------------------------------------------------

implicit<s> val get : s
implicit<s> fun set(x : s) : ()

fun local( init, action ) { // : (a,() -> <set<a>,get<a>|e> b) -> e b
  with val get = init
  with control set(x){
    with val get = x
    // change effect of `resume(())` from `<get<a>|e>` to `<get<a>,get<c>|e>`.
    // this has no runtime effect but is needed for type inference.
    inject behind<get>{resume(())}
  }
  action()
}

fun printn() {
  while{ get > 0 } {
    println((get:int))
    set(get - 1)
  }
}

fun main() {
  with local(5)
  printn()
}
