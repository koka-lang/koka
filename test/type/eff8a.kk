fun forever(f) {
  f();
  print("hi");
  forever(f);
}