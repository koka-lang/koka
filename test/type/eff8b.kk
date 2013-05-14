fun forever(f) {
  f("hi");
  error("hi");
  forever(f);
}

fun test() {
  forever( print );
}