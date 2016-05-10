// --------------------------------------------------------
// Definition of players
// --------------------------------------------------------

type player {
  Bob 
  Alice
}

function (==)(p1, p2) {
  match(p1) {
    Bob   -> p2.isBob
    Alice -> p2.isAlice
  }
}

function show(p:player) : string {
  match(p) {
    Bob   -> "bob"
    Alice -> "alice"
  }
}

function show(ps : list<player> ) : string {
  ps.showList(show)
}


// --------------------------------------------------------
// Game of Nim
// --------------------------------------------------------

effect nim {
  move(player:player,sticks:int) : int
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

// --------------------------------------------------------
// Perfect strategy
// --------------------------------------------------------

val perfect = 
  handler {
    return x  -> x
    move(p,n) -> resume(max(1,n%4))
  }

function testPerfect1() {
  perfect{ game(7) }  // alice
}
function testPerfect2() {
  perfect{ game(12) } // bob
}


// --------------------------------------------------------
// Game trees
// --------------------------------------------------------

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

function showGt(gt :gtree, indent:int) : _ string {
  val showi = (show : (int) -> string)
  match(gt) {
    Take(p,moves) -> p.show + moves.showList(fun(x) { "\n" + string(indent,' ') +  x.fst.core/show + " -> " + x.snd.showGt(indent+2) })
    Winner(p)     -> p.show + " wins"
  }
}

function testGt() {
  gametree{ game(3) }  // figure 1 in the paper
}


// --------------------------------------------------------
// Cheating
// --------------------------------------------------------

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


// --------------------------------------------------------
// Cheating strategy
// --------------------------------------------------------

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
  // the `perfect` on the outside is necessary as the type system
  // does not detect that `check` will never perform a `move`.
}


// --------------------------------------------------------
// Choosing a strategy
// --------------------------------------------------------

effect choose {
  choose() : bool
}

function bobChooses(m) {
  if (choose()) then pc(m) else perfect(m)
}

val allResults = handler {
  return x -> [x]
  choose() -> resume(True) + resume(False)
}

function testChoose() {
  allResults{ bobChooses { game(7) } }  // [bob,alice]
}

val coin = handler {
  return x -> x
  choose() -> resume(random() > 0.5)
}

function testCoin() {
  coin{ bobChooses { game(7) } }
}


// --------------------------------------------------------
// Scoreboard
// --------------------------------------------------------

effect state<s> {
  get() : s;
  put(x : s) : ()
}

val state = handler(s) {
  return x -> x
  get()    -> resume(s,s)
  put(t)   -> resume(t,())
}

alias gstate = list<(player,int)>

val s0 = [(Alice,0),(Bob,0)]

val scoreUpdater = handler {
  return p -> {
    put(updateScore(p,get()))
    p
  }
}
function updateScore( p : player, gs : gstate ) : gstate {
  gs.map fun(qs) {
    val (q,s) = qs
    if (p==q) then (q,s+1) else qs
  }
}

val printer = handler {
  return x -> {
    printBoard(get())
    x
  }
}

function printBoard( gs : gstate ) : io () {
  gs.map( fun(ps) {
    ps.fst.show + " -> " + ps.snd.show 
  }).join("\n").println
}

function testPrint1() {
  state(s0){ printer{ coin{ bobChooses{ scoreUpdater{ game(7) }}}}}
}

function replay(n : int,action : () -> <div|e> a) : <div|e> a {
  handle(action) {
    return x -> if (n <= 1) then x else replay(n-1, incps(action))
  }
}

external inline incps( action: () -> e a ) : total (() -> <cps|e> a) = inline "#1"

function testPrint2() {
  state(s0){ printer{ replay(10){ coin{ bobChooses{ scoreUpdater{ game(7) }}}}}}
}

// the following fails to type check due to the recursive 'cps' effect occurring in 'action'
/* 
function replay(n : int,action : () -> <div,cps|e> a) : <div,cps|e> a {
  handle(action) {
    return x -> if (n <= 1) then x else replay(n-1,action)
  }
}
*/

function main() {
  [""
  ,testPerfect1().show 
  ,testPerfect2().show
  ,testGt().show
  ,testCheck().show
  ,testPc2().show
  ,testChoose().show
  ,testCoin().show
  ,testPrint1().show
  ,testPrint2().show
  ,""
  ].join("\n-----------\n").print
  testPc1().show.println // raises exception
}