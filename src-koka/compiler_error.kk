
alias rho = ktype // unqualified type

type ktype
  TForAll(r: rho)
  Default()

fun maxSynonymRank(t: ktype): int
  match t
    TForAll(r) -> maxSynonymRank(r)
    Default() -> 0

fun main(): <console> ()
  val h = TForAll(Default)
  maxSynonymRank(h).println
