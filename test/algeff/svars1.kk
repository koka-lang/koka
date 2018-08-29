// Tests mutable stack variables

abstract type lvar<s::S,a> {
  con Lvar
}

effect local<s::S,a::V> {
  fun get(l : lvar<s,a> ) : a
  fun set(l : lvar<s,a>, x : a ) : ()
}

fun local( init : a, action : forall<s> (lvar<s,a>) -> <local<s,a>|e> b) : e b {
  handle({action(Lvar)}) (s = init) {
    get(_l)   -> resume(s,s)
    set(_l,x) -> resume((),x)
  }
}


fun test1() {
  use x = local(42) 
  x.get
}


effect tick {
  fun tick() : int
}

fun ticks(action : () -> <tick|e> a ) : e a {
  use n = local(0)
  handle(inject<local>(action)) {
    tick() -> {
      val x = n.get
      n.set(x + 1)
      resume(x)
    }
  }
}

fun test-tick() {
  using ticks
  tick()
  tick()
  tick()
}

