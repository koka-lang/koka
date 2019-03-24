// see: https://wiki.haskell.org/Extensible_datatypes

type open tpA;

alias tpB = int;

type extend tpA {
  B( b : tpB )
}

fun up( b : tpB ) : tpA {
  B(b)
}

fun down(a : tpA) : maybe<tpB> {
  match(a) {
    B(b) -> Just(b)
    _    -> Nothing
  }
}

fun main() {
  up(42).down.maybe(0,id).print
}