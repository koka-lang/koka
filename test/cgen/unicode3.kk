import std/text/unicode

val strings = [
    "한",
    "한글",
    "x\u0307\u0323x",
    "깍",
    // "Tëst 👨‍👩‍👦 🇺🇸 नी நி!"
]

fun info( s : string ) 
  println( "count: " ++ s.count.show ++ 
          ", width: " ++ s.width.show ++
          ", grapheme len: " ++ s.graphemes.length.show ++ ",") 
  println("    graphemes: " ++ s.graphemes.show-list(fn(g) g.list.show-list(fn(c) c.show) ) ++
          ", unicodes: " ++ s.list.show-list(fn(i) show-hex(i.int))
         )


pub fun main() 
  strings.foreach(info)

