fun runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  { throw("runST") }

fun ok() {
  runST( fn() { 1 } )
}