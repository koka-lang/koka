module time12

import std/num/ddouble
import std/time

pub fun main()
  val tstart = now()
  val tend = now()
  val time = (tend - tstart).milli-seconds
  time.println