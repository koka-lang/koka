val s0 = "x\u2603x (snowman)"
val s1 = "x\U01F4AAx (astral )"
val s2 = "x\U01D400x (math A )"

fun test1(s : string,ofs = 0) {
  println( "count: " + (s.count.show + 
          ", sub3: " + s.first.advance(ofs).extend(2).string + 
          ", rev3: " + s.last.advance(~12).extend(2).string +
          ",\n  s: " + s +
          ", " + s.list.show-list(fun(c){show-hex(c.int)})) )
}

fun main() {
  s0.test1
  s1.test1
  s2.test1
}
