// --------------------------------------------------------
//
// --------------------------------------------------------
module file1

public import std/file
public import std/path
public import std/xtime

public fun main() {
  val f = path("readme.md")
  f.mod-time.unix-instant.local-time.show.println
  // f.read-text.println
  val l = "https://www.ietf.org/timezones/data/leap-seconds.list"
  download-text(l,1.0).println
}
