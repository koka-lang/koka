// test subtyping etc
function ioapp( f ) :  () {
  return f()
}

function test() {
  ioapp(fun(){ () })
}
