fun whilex(p,f) {
  if (p()) then {
    f();
    whilex(p,f);
  }
  else {
    print("done");
    ()
  }
}