// shadow definition
function id(x) {
  function id2(x) -> x
  function id(y) -> id2(y)
  id(x)
}
