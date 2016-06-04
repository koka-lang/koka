function id(x) {
  return x;
}

function apply(f,x) {
  f(x)
}

function test() {
  fun i(x) { x }
  i(i)
}

function test2() {
  single(hm1/id)
}

function test3()  -> choose(fun(x,y){x},fun(x,y){y})
function test4(f) -> choose(core/id,f)
