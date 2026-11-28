// Used by `stack test --target-wasm` to pre-compile the standard libraries (without async as that is not yet supported on wasm)
import std/core/types
import std/core/unsafe
import std/core/undiv
import std/core
import std/os/env
import std/os/flags
import std/os/file
import std/os/dir
import std/os/path
import std/os/process
import std/os/readline
import std/os/task
import std/text/parse
import std/text/regex
import std/text/unicode
import std/num/int32
import std/num/int64
import std/num/float64
import std/num/decimal
import std/num/ddouble
import std/num/random
import std/time
import std/time/timestamp
import std/time/astro

fun main() : console ()
  println("done.")
