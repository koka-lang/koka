effect amb {
  fun flip() : bool
}

val amb = handler {
  return x -> [x]
  fun flip() {
    resume(True) + resume(False)
  }
}

effect tick {
  fun tick() : int
}

fun lticker( action : () -> <tick|e> a ) : e a {
  use s = local(0)  
  handle(inject-local(action)) {
    fun tick() {
      s := s + 1
      resume(s)
    }
  }
}

fun rticker( action : () -> <tick|e> a ) : e a {
  val s = ref(0)
  handle(inject-st(action)) {
    fun tick() {
      s := !s + 1
      resume(!s)
    }
  }
}

fun foo() {
  tick()
  flip()
  tick()
}

fun ltest() {
  using amb
  using lticker
  foo()
}

fun rtest() {
  using amb
  using rticker
  foo()
}

fun main() {
  ltest().show-list(show).println   // [2, 2]
  rtest().show-list(show).println   // [2, 3]
}
