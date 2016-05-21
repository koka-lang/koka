
function foo() {
  val x = if (True || False) then 42 else return 32
  x
}

function main() {
  foo().print
}
