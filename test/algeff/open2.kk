// test lifting of cps functions
function myid() : ((x:a) -> e a) {
  return fun(x){ x }
}

type func<e::E> {
  Func (f : int -> e int)
}

val myfunc = Func(myid())

function bar() {
  match(myfunc) {
    Func(f) -> f(42)
  }
}

function main() {
  bar().show.println
}