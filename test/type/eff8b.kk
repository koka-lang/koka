fun forever(f) {
  f("hi");
  throw("hi");
  forever(f);
}

fun test() {
  forever( print );
}