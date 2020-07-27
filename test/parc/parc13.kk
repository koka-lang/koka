fun test(x:int): (() -> int) {
  return fun() { x }
}
