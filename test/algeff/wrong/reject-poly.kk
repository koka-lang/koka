effect get-id {
  control get-id-op(): (a -> a) 
}

fun test() {
  val f : forall<a> a -> a = get-id-op()     // can we generalize the get-id-op result $a -> $a ?
  if (f(True)) then (f(0) + 1) else 2
}

val h = handler {
  control get-id-op() { 
    resume( fn(z){ resume( fn(w){ z }); z } ) 
  }
}

fun main() {
  h(test) // 1
}