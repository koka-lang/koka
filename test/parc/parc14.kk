fun test(x:int, y:a, z:a): a {
  match(x) {
    0 -> /* drop x, z */ y
    _ -> /* drop x, y */ z
  }
}
