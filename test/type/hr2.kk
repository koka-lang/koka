fun runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  { error("runST") }

fun ok() {
  runST( fn() { 1 } )
}