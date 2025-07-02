import std/os/path
import std/os/dir

fun main()
  val files = path("test/lib/dir").list-directory
  println("found " ++ files.length.show ++ " files")
