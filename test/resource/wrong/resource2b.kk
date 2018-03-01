// Test scoped handlers

effect heap<s::S> { 
  fun get-ref() : ref<s>
}

type ref<s::S> {
  Ref
}

fun get( r : ref<s> ) : heap<s> int {
  42
}

fun heap( action : forall<s> () -> <heap<s>|e> a) : e a {
  handle(action) { 
    get-ref() -> resume(Ref)
  }
}

fun foo() {
  using heap
  get-ref()
}

fun bar() {
  using heap
  foo().get   // wrong: trying to access reference out of scope
}

fun main() {
  println(bar())
}
