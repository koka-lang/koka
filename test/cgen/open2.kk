// see: https://wiki.haskell.org/Extensible_datatypes

type open tpA;

alias tpB = int;

type extend tpA {
  B( b : tpB )
}

function up( b : tpB ) : tpA {
  B(b)
}

function down(a : tpA) : maybe<tpB> {
  match(a) {
    B(b) -> Just(b)
    _    -> Nothing
  }
}

function main() {
  up(42).down.maybe(0,id).print
}