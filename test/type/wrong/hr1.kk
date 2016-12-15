function runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  {
  error("runST")
}

function wrong() {
  val r = ref(1)
  runST( fun() { !r } )
}

function main() {
  print("hi")
}