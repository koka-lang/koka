// --------------------------------------------------------
// Examples from the paper "Liberating effects with rows and handlers"
// by Daniel Hillerström and Sam Lindley.
// --------------------------------------------------------

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
  ps.show-list(show)
}


// --------------------------------------------------------
// Game of Nim
// --------------------------------------------------------

ambient control move(player:player,sticks:int) : int


fun aliceTurn(n : int) : <div,move> player {
  if (n <= 0) then Bob else bobTurn( n - move(Alice,n) )
}

fun bobTurn(n : int) : <div,move> player {
  if (n <= 0) then Alice else aliceTurn( n - move(Bob,n) )
}

fun game(n) {
  aliceTurn(n)
}

// --------------------------------------------------------
// Perfect strategy
// --------------------------------------------------------

val perfect = with fun move(_p,n) { max(1,n%4) }

fun testPerfect1() {
  with perfect
  game(7)        // alice
}
fun testPerfect2() {
  with perfect
  game(12)  // bob
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

val gametree = with {
  return x  { Winner(x) }
  control move(p,n) {
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
    Take(p,moves) -> p.show + moves.show-list(fun(x) { "\n" + " ".repeat(indent) +  x.fst.core/show + " -> " + x.snd.showGt(indent+2) })
    Winner(p)     -> p.show + " wins"
  }
}

fun testGt() {
  with gametree
  game(3)   // figure 1 in the paper
}


// --------------------------------------------------------
// Cheating
// --------------------------------------------------------

ambient fun cheat(player:player) : a

val cheatReport =  with fun cheat(p) { error(p.show + " cheated!") }

val check = with control move(p,n) {
  val m = move(p,n)
  if (validMoves(n).find(fun(i) { i == m }).bool)
   then resume(m)
   else cheat(p)
}


fun testCheck() {
  with perfect
  with cheatReport
  with check
  game(7)  // alice
}

fun testuse() {
  with x = map([1,2,3])
  x+2
}
// --------------------------------------------------------
// Cheating strategy
// --------------------------------------------------------

val pc = with fun move(p,n) {
  match(p){
    Alice -> max(1,n%4)
    Bob   -> n
  }
}

fun testPc1() {
  //  pc { cheatReport { check{ game(12) } } }  // exn: bob cheats
  with pc
  with cheatReport
  with check
  game(12)
}

fun testPc2() {
  // the `perfect` on the outside is necessary as the type system
  // does not detect that `check` will never perform a `move`.
  // perfect{ cheatReport { check{ pc { game(12) }  } } }  // bob
  with perfect
  with cheatReport
  with check
  with pc
  game(12)
}



// --------------------------------------------------------
// Choosing a strategy
// --------------------------------------------------------

ambient control choose() : bool

fun bobChooses(m) {
  if (choose()) then pc(m) else perfect(m)
}

val allResults = with {
  return(x)       { [x] }
  control choose() { resume(True) + resume(False) }
}

fun testChoose()  {
  with control choose() { resume(True) + (resume(False) : list<_>) }  // TODO: improve inference
  with bobChooses
  [game(7)]   // [bob,alice]
}

val coin = with fun choose() { random() > 0.5 }

fun testCoin() {
  with fun choose() { random() > 0.5 }
  with bobChooses
  game(7)
}


// --------------------------------------------------------
// Scoreboard
// --------------------------------------------------------

ambient state<s> {
  fun get() : s;
  fun put(x : s) : ()
}


val state-param = handler(s) {
  get()    -> resume(s,s)
  put(t)   -> resume((),t)
}


fun state(init,action) {
  var s := init
  with {
    fun get()  { s }
    fun put(x) { s := x }
  }
  action()
}

alias gstate = list<(player,int)>

val s0 = [(Alice,0),(Bob,0)]

val scoreUpdater = with return(p) { put(updateScore(p,get())); p }

fun updateScore( p : player, gs : gstate ) : gstate {
  gs.mymap fun(qs) {
    val (q,s) = qs
    if (p==q) then (q,s+1) else qs
  }
}

val printer = with return(x) { printBoard(get()); x }

fun printBoard( gs : gstate ) : io () {
  gs.mymap( fun(ps) {
    ps.fst.show + " -> " + ps.snd.show
  }).join("\n").println
}

fun testPrint1() {
  with state(s0)
  with printer
  with coin
  with bobChooses
  with scoreUpdater
  game(7)
}

fun replay(n : int, action : () -> <div|e> a) : <div|e> a {
  with return(x) { if (n <= 1) then x else replay(n - 1, action ) }
  action()
}

fun testPrint2() {
  with state(s0)
  with printer
  with replay(10)
  with coin
  with bobChooses
  with scoreUpdater
  game(7)
}


fun main() {
  [""
  ,testPerfect1().show
  ,testPerfect2().show
  ,testGt().show
  ,testCheck().show
  ,testPc2().show
  ,testChoose().show
  //,testCoin().show
  //,testPrint1().show
  //,testPrint2().show
  ,""
  ].join("\n-----------\n").print
  testPc1().show.println // raises exception
}


fun mymap( xs, f ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mymap(xx,f))
  }
}
