// see: https://wiki.haskell.org/Extensible_datatypes

open type tpA;

alias tpB = int;

extend type tpA {
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
