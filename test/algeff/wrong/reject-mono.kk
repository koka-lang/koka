effect get-id {
  control get-id-op() : (a -> a) 
}

fun test() {
  val f = get-id-op()
  if (f(1)==1) then (f(0) + 1) else 2
}

val h = handler {
  control get-id-op() { 
    resume( fn(z){ resume( fn(w){ z }); z } ) 
  }
}

fun main() {
  h(test) // 1
}