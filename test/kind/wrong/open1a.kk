module wrong/open1a

import open0

extend type test {
  X
}

function test( x : test ) {
  match(x) {
    open0/X -> "original X"
    open1a/X -> "the new X"
  }
}