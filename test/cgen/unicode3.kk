import std/text/unicode

val s0 = "한"
val s1 = "한글"  
val s2 = "x\u0307\u0323x" 
val s3 = "깍"

fun test1(s : string,ofs = 1) {
  println( "count: " + s.count.show + 
          ", width: " + s.width.show +
          ", grapheme len: " + s.graphemes.length.show + 
          ",\n    graphemes: " + s.graphemes.show-list(fun(g){g.list.show-list(fun(c){c.show})}) +
          ", unicodes: " + s.list.show-list(fun(i){show-hex(i.int)})
         )
}

fun main() {
  s0.test1
  s1.test1
  s2.test1
  s3.test1
}
