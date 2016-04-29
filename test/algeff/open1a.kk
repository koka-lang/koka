// test lifting of cps functions
function myid() : ((x:a) -> e a) {
  return fun(x){ x }
}

val hs : forall<e> list<int -> e int> = [id]

function bar(x:int) : <exn> int {
  head(hs)(x)
}
