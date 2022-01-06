// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

pub import std/os/file
pub import std/os/path
pub import std/time

pub fun main() {
  val f = path("readme.md")
  println(f.mod-time > instant(2016,5,15))
  // f.read-text.println
  val l = "https://www.ietf.org/timezones/data/leap-seconds.list"
  download-text(l,duration(1)).first(594).string.println
}
