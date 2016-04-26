function while(p,f) {
  if (p()) then {
    f();
    while(p,f);
  }
  else {
    print("done");
    ()
  }
}