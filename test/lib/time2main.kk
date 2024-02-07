module time2main

@open import time2
import std/core
import std/time/utc

pub fun @expr() : _ ()
  time2/main()

pub fun main() : io-noexn ()
  std/time/utc/@default-utc(fn() std/core/@default-exn(fn() @expr()))