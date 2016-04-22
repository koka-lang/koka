public module open1a

import open0

val theY = open0/Y
val theX = open0/X

extend type test {
  X
}

function test( x : test ) {
  match(x) {
    open0/X -> "original X"
    open1a/X -> "the new X"
  }
} 