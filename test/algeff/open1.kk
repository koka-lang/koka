// test lifting of cps functions
function myid() : ((x:a) -> e a) {
  return fun(x){ x }
}

val hs : forall<e> list<int -> e int> = [myid()]

function bar() {
  head(hs)(10)
}
