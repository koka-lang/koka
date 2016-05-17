// test duplicate cps translation of polymorphic results

function compose1(f,g) {
  function comp(x) {
    f(g(x))
  }
  comp
}


function compose2(f,g) {
  return fun(x) {
    f(g(x))
  }
}

function inc(x:int) {
  x+1
}

function main() {
  println(compose1(inc,inc)(42))
  println(compose2(inc,inc)(42))
}
