// --------------------------------------------------------------
// Local mutable state defined as ambient control.
// This is a much nicer way to give semantics to state
// versus the state-monad encoding. However, implementations
// need to support frame "smashing" to implement this in constant
// stack space.
// --------------------------------------------------------------

ambient<s> val get : s
ambient<s> control set(x : s) : ()

fun local( init, action ) { // : (a,() -> <set<a>,get<a>|e> b) -> e b
  with val get = init
  with control set(x){
    mask<get> {
      with val get = x
      resume(())
    }
  }
  action()
}

fun localo( init, action ) { // : (a,() -> <set<a>,get<a>|e> b) -> e b
  with val get = init
  with control set(x){
    with override val get = x
    resume(())
  }
  action()
}

fun printn() {
  while{ get > 0 } {
    println((get:int))
    set(get - 1)
  }
}

fun test1() {
  with local(5)
  printn()
}

fun test2() : int {
  // this tests if the inject behind is working correctly:
  with val get = 41
  with local(0)
  set(get + 1)
  (0 + get + mask<get>{get})  // = 42!
}

// If we make the effect row closed, we can guarantee absence of
// `inject<get>` expressions in the action and thus make it safe
// without using the `inject behind` primitive.
fun local-closed1( init : s, action : () -> <get<s>,get<s>,set<s>> a ) : <get<s>> a {
  with val get = init
  with control set(x) {
    with val get = x
    fun k() : <get<_>,get<_>> _ { resume(()) } // help type inference... :-(
    k()
  }
  action()
}

fun local-closed2( init : s, action : () -> <get<s>,set<s>> a ) : <> a {
  with val get = init
  with control set(x) {
    with val get = x
    fun k() : <get<_>> _ { resume(()) }  // help type inference ... :-(
    k()
  }
  action()
}


fun main() {
  test1()
  test2().println
}
