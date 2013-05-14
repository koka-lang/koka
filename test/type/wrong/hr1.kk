fun runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  = error("runST")

fun wrong() {
  r = ref(1)
  runST( fun() { !r } )
}

fun main() {
  print("hi")
}