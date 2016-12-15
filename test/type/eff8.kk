function forever(f) {
  f();
  forever(f);
}