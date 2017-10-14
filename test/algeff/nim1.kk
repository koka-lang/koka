// --------------------------------------------------------
// Definition of players
// --------------------------------------------------------

type player {
  Bob 
  Alice
}

fun (==)(p1, p2) {
  match(p1) {
    Bob   -> p2.bob?
    Alice -> p2.alice?
  }
}

fun show(p:player) : string {
  match(p) {
    Bob   -> "bob"
    Alice -> "alice"
  }
}

fun show(ps : list<player> ) : string {
  ps.showList(show)
}


// --------------------------------------------------------
// Game of Nim
// --------------------------------------------------------

effect nim {
  move(player:player,sticks:int) : int
}


fun aliceTurn(n) {
  if (n <= 0) then Bob else bobTurn( n - move(Alice,n) )
}

fun bobTurn(n) {
  if (n <= 0) then Alice else aliceTurn( n - move(Bob,n) )
}

fun game(n) {
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

fun testPerfect1() {
  perfect{ game(7) }  // alice
}
fun testPerfect2() {
  perfect{ game(12) } // bob
}


// --------------------------------------------------------
// Game trees
// --------------------------------------------------------

type gtree {
  Take( player: player, moves: list<(int,gtree)> )
  Winner( player: player )
}

fun validMoves(n) {
  [1,2,3].filter( fun(m) { m <= n })
}

val gametree = handler {
  return x  -> Winner(x)
  move(p,n) -> {
    val moves    = validMoves(n)
    val subgames = moves.mymap(resume)
    val subtrees = zip(moves,subgames)
    Take(p,subtrees)
  }
}

fun show(gt :gtree) : div string {
  showGt(gt,2)
}

fun showGt(gt :gtree, indent:int) : _ string {
  val showi = (show : (int) -> string)
  match(gt) {
    Take(p,moves) -> p.show + moves.showList(fun(x) { "\n" + string(indent,' ') +  x.fst.core/show + " -> " + x.snd.showGt(indent+2) })
    Winner(p)     -> p.show + " wins"
  }
}

fun testGt() {
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

fun testCheck() { 
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

fun testPc1() { 
  pc { cheatReport { check{ game(12) } } }  // exn: bob cheats
}

fun testPc2() { 
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

fun bobChooses(m) {
  if (choose()) then pc(m) else perfect(m)
}

val allResults = handler {
  return x -> [x]
  choose() -> resume(True) + resume(False)
}

fun testChoose() {
  allResults{ bobChooses { game(7) } }  // [bob,alice]
}

val coin = handler {
  return x -> x
  choose() -> resume(random() > 0.5)
}

fun testCoin() {
  coin{ bobChooses { game(7) } }
}


// --------------------------------------------------------
// Scoreboard
// --------------------------------------------------------

effect linear state<s> {
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
fun updateScore( p : player, gs : gstate ) : gstate {
  gs.mymap fun(qs) {
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

fun printBoard( gs : gstate ) : io () {
  gs.mymap( fun(ps) {
    ps.fst.show + " -> " + ps.snd.show 
  }).join("\n").println
}

fun testPrint1() {
  state(s0){ printer{ coin{ bobChooses{ scoreUpdater{ game(7) }}}}}
}

fun replay(n : int,action : () -> <div|e> a) : <div|e> a {
  handle(action) {
    return x -> if (n <= 1) then x else replay(n - 1, action)
  }
}

fun testPrint2() {
  state(s0){ printer{ replay(10){ coin{ bobChooses{ scoreUpdater{ game(7) }}}}}}
}


fun main() {
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

fun mymap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mymap(f,xx))
  }
}