import std/os/file-async
import std/os/path
import std/async

fun main()
  "Read file\n\n".println
  val res = read-as-string("stack.yaml".path)
  res.println
  "\n\nDone!".println
  "Write file\n\n".println
  val file = open("scratch/test.txt".path, o_RDWR.or(o_CREAT), s_IRWXU)
  write-to-file(file, "Hello, world!")
  "Done!".println