function forever(f) {
  f("hi");
  error("hi");
  forever(f);
}

function test() {
  forever( print );
}