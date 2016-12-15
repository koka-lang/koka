// test subtyping etc
function ioapp( f ) : _e () {
  print("hi")
  return f()
}

function test() {
  ioapp(fun(){ () })
}
