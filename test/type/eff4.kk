// test subtyping etc
function ioapp( f ) : io () {
  return f()
}

function test() {
  ioapp(fun(){ () })
}
