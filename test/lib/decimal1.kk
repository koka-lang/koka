module decimal1

import std/num/decimal

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}

public fun main() {
  check("trunc1","0"){ 0.5.decimal.truncate.show }
  check("round1","1"){ 1.decimal.round(Away-from-zero).show }
}