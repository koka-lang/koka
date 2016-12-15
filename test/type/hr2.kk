function runST( action : forall<h> () -> <st<h>,exn> a ) : exn a  { error("runST") }

function ok() {
  runST( fun() { 1 } )
}