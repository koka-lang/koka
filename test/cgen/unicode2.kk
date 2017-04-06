val s0 = "ma\xF1ana"     // single character
val s1 = "man\u0303ana"  // combining character
val s2 = "x\u0307\u0323xyz" // combining mark

fun test1(s : string,ofs = 1) {
  println( "count: " + (s.count.show + 
          ", sub3: " + s.first.advance(ofs).extend(2).string + 
          ", rev3: " + s.last.advance(~4).extend(2).string +
          ",\n  s: " + s +
          ", " + s.list.show-list(fun(i){show-hex(i.int)})) )
}

fun main() {
  s0.test1
  s1.test1
  s2.test1
}
