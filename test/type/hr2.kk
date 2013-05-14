fun runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  = error("runST")

fun ok() = runST( fun() { 1 } )