// https://github.com/koka-lang/koka/issues/457
// https://github.com/koka-lang/koka/issues/458
import std/text/unicode

fun main()
  //           heart, variation, zero width join, fire
  // ['h','i','/u2764','/uFE0F','/u200D','/U01F525']
  "hi❤️‍🔥".list.println
  "hi❤️‍🔥".graphemes.length.println
  
  println(width("👾"))
