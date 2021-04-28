fun runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  {
  throw("runST")
}

fun wrong() {
  val r = ref(1)
  runST( fn(){ !r } )
}

fun main() {
  print("hi")
}