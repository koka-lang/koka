val s0 = "ma\xF1ana"     // single character
val s1 = "man\u0303ana"  // combining character
val s2 = "x\u0307\u0323xyz" // combining mark

fun test1(s : string,ofs = 1) {
  println( "len: " + s.length.show + 
          ", sub3: " + s.substr(ofs,3) + 
          ", rev3: " + s.substr(~5,3) +
          ",\n  s: " + s +
          ", " + s.list.show-list(fun(i){show-hex(i.int)}) )
}

fun main() {
  s0.test1
  s1.test1
  s2.test1
}
