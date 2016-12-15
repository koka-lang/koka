// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

public import std/os/file
public import std/os/path
public import std/time

public fun main() {
  val f = path("readme.md")
  println(f.mod-time > instant(2016,5,15))
  // f.read-text.println
  val l = "https://www.ietf.org/timezones/data/leap-seconds.list"
  download-text(l,duration(1)).first(594).string.println
}
