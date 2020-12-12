fun test(x:int): (() -> int) {
  return fn() { x }
}
