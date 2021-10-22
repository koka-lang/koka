struct id{ value : forall<a> a -> a }  

effect get-id {
  control get-id-op() : id
}

fun test() {
  val Id(g) = get-id-op()
  val f : forall<a> a -> a = g // foo() //get-id-op()
  if (f(True)) then (f(0) + 1) else 2
}

val h = handler {
  rawctl get-id-op() { 
    rcontext.resume( Id( fn(z){ rcontext.resume( Id( fn(w){ w } )); z }) ) 
  }
}

fun main() {
  h(test)  // 1
}