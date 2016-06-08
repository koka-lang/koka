val s0 = "x\u2603x (snowman)"
val s1 = "x\U01F4AAx (astral )"
val s2 = "x\U01D400x (math A )"

fun test1(s : string,ofs = 0) {
  println( "len: " + s.length.show + ", sub3: " + s.substr(ofs,3) + ", s: " + s)
}

fun main() {
  s0.test1
  s1.test1
  s2.test1
}
