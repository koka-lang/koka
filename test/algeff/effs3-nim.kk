import effs3

type player {
  Bob 
  Alice
}

effect nim {
  move(player:player,sticks:int) : int
}


function show(p:player) {
  match(p) {
    Bob -> "bob"
    Alice -> "alice"
  }
}

function aliceTurn(n) {
  if (n <= 0) then Bob else bobTurn( n - move(Alice,n) )
}

function bobTurn(n) {
  if (n <= 0) then Alice else aliceTurn( n - move(Bob,n) )
}

function game(n) {
  aliceTurn(n)
}

function perfect(m) {
  val h = handler {
    return x  -> x
    move(p,n) -> resume(max(1,n%4))
  }
  h({game(m)})
}