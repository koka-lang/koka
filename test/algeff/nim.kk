type player {
  Bob 
  Alice
}

effect nim {
  move(player:player,sticks:int) : int
}


function show(p:player) : string {
  match(p) {
    Bob   -> "bob"
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

val perfect = 
  handler {
    return x  -> x
    move(p,n) -> resume(max(1,n%4))
  }

// game trees
type gtree {
  Take( player: player, moves: list<(int,gtree)> )
  Winner( player: player )
}

function validMoves(n) {
  [1,2,3].filter( fun(m) { m <= n })
}

val gametree = handler {
  return x  -> Winner(x)
  move(p,n) -> {
    val subgames = validMoves(n).map(resume)
    val subtrees = zip([1,2,3],subgames)
    Take(p,subtrees)
  }
}

function show(gt :gtree) : div string {
  showGt(gt,2)
}

val showi = (show : (int) -> string)

function showGt(gt :gtree, indent:int) : _ string {
  match(gt) {
    Take(p,moves) -> p.show + moves.showList(fun(x) { "\n" + string(indent,' ') +  x.fst.core/show + " -> " + x.snd.showGt(indent+2) })
    Winner(p)     -> p.show + " wins"
  }
}

// cheating

effect cheating {
  cheat<b>(player:player) : b
}

val cheatReport = handler {
  return x -> x
  cheat(p) -> error(p.show + " cheated!")
}

val check = handler {
  return x  -> x
  move(p,n) -> {
    val m = move(p,n)
    if (validMoves(n).find(fun(i) { i == m }).bool)
     then resume(m)
     else cheat(p)
  }
}

function testCheck() { 
  perfect{ cheatReport { check{ game(7) }  } } // alice
}


val pc = handler {
  return x -> x
  move(p,n) -> match(p){
    Alice -> resume(max(1,n%4))
    Bob   -> resume(n)
  }
}

function testPc1() { 
  pc { cheatReport { check{ game(12) } } }  // exn: bob cheats
}

function testPc2() { 
  perfect{ cheatReport { check{ pc { game(12) }  } } }  // bob
}
