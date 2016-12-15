// test subtyping etc
function ioapp( f : () -> <io|e> () ) : <io|e> () {
  return f()
}

function test() {
  ioapp(fun(){ () })
}
