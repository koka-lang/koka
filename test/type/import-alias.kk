// https://github.com/koka-lang/koka/issues/705
import foo = std/os/path

fun main()
  println(path/cwd())
  println(foo/cwd())