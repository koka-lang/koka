/* Examples from the paper "Liberating effects with rows and handlers"
   by Daniel Hillerström and Sam Lindley.
   <https://www.dhil.net/research/papers/liberating_effects-tyde2016.pdf>
*/
module nim

// --------------------------------------------------------
// Definition of players
// --------------------------------------------------------
import std/num/random

type player
  Bob
  Alice


fun (==)(p1, p2)
  match (p1,p2)
    (Bob,Bob)     -> True
    (Alice,Alice) -> True
    _             -> False


pub fun player/show(p:player) : string
  match p
    Bob   -> "bob"
    Alice -> "alice"


pub fun playerlist/show(ps : list<player> ) : string
  ps.show-list(show)



// --------------------------------------------------------
// Game of Nim
// --------------------------------------------------------

effect ctl move( player :player, sticks :int) : int


fun alice-turn(n : int) : <div,move> player
  if n <= 0 then Bob else bob-turn( n - move(Alice,n) )


fun bob-turn(n : int) : <div,move> player
  if n <= 0 then Alice else alice-turn( n - move(Bob,n) )


fun game(n)
  alice-turn(n)


// --------------------------------------------------------
// Perfect strategy
// --------------------------------------------------------

val perfect = handler fun move(_p,n){ max(1,n%4) }

pub fun example-perfect1()
  with perfect
  game(7)        // alice


pub fun example-perfect2()
  with perfect
  game(12)  // bob



// --------------------------------------------------------
// Game trees
// --------------------------------------------------------

type gtree
  Take( player: player, moves: list<(int,gtree)> )
  Winner( player: player )


fun valid-moves(n)
  [1,2,3].filter(fn(m) m <= n)


val gametree = handler
  return(x)  Winner(x)
  ctl move(p,n)
    val moves    = valid-moves(n)
    val subgames = moves.map(resume)
    val subtrees = zip(moves,subgames)
    Take(p,subtrees)


pub fun show(gt :gtree) : div string
  show-gtree(gt,2)


fun show-gtree(gt :gtree, indent:int) : _ string
  match gt
    Winner(p) -> p.show ++ " wins"
    Take(p,moves) ->
      val lines = moves.map fn((sticks,gtx))
                    "\n" ++ " ".repeat(indent) ++ sticks.show
                      ++ " -> " ++ gtx.show-gtree(indent+2)
      p.show ++ lines.join


pub fun example-gtree()
  with gametree
  game(3)   // figure 1 in the paper



// --------------------------------------------------------
// Cheating
// --------------------------------------------------------

effect fun cheat(player:player) : a

val cheat-report = handler
  fun cheat(p)
    throw(p.show ++ " cheated!")

val check = handler
  ctl move(p,n)
    val m = move(p,n)
    if (valid-moves(n).find(fn(i) i == m).bool)
      then resume(m)
      else cheat(p)


pub fun example-check()
  with perfect
  with cheat-report
  with check
  game(7)  // alice


pub fun example-use()
  with x = map([1,2,3])
  x+2

// --------------------------------------------------------
// Cheating strategy
// --------------------------------------------------------

val pc = handler
  fun move(p,n)
    match p
      Alice -> max(1,n%4)
      Bob   -> n


pub fun example-pc1()
  with pc
  with cheat-report
  with check
  game(12)


pub fun example-pc2()
  // the `perfect` on the outside is necessary as the type system
  // does not detect that `check` will never perform a `move`.
  with perfect
  with cheat-report
  with check
  with pc
  game(12)




// --------------------------------------------------------
// Choosing a strategy
// --------------------------------------------------------

effect ctl choose() : bool

fun bob-chooses(m)
  if choose() then pc(m) else perfect(m)


val all-results = handler
  return(x)    [x]
  ctl choose() resume(True) ++ resume(False)


pub fun example-choose()
  with ctl choose() resume(True) ++ (resume(False):list<_>)
  with bob-chooses
  [game(7)]   // [bob,alice]


val coin = handler
  fun choose() srandom-float64() > 0.5

pub fun example-coin()
  with coin
  with bob-chooses
  game(7)



// --------------------------------------------------------
// Scoreboard
// --------------------------------------------------------

effect state<s>
  fun get() : s;
  fun put(x : s) : ()



fun state(init,action)
  var s := init
  handle(action)
    fun get()  s
    fun put(x) s := x


alias gstate = list<(player,int)>

val s0 = [(Alice,0),(Bob,0)]

val score-updater = handler
  return(p){ put(update-score(p,get())); p }

fun update-score( p : player, gs : gstate ) : gstate
  gs.map( fn((q,s)) if p==q then (q,s+1) else (q,s) )

val printer = handler
  return(x){ print-board(get()); x }

fun print-board( gs : gstate ) : io ()
  val scores = gs.map fn((p,s)) p.show ++ " -> " ++ s.show
  scores.join("\n").println


pub fun example-print1()
  with state(s0)
  with printer
  with coin
  with bob-chooses
  with score-updater
  game(7)


fun replay(n : int, action : () -> <div|e> a) : <div|e> a
  with return(x) if n <= 1 then x else replay(n - 1, action )
  action()


pub fun example-print2()
  with state(s0)
  with printer
  with replay(10)
  with coin
  with bob-chooses
  with score-updater
  game(7)



pub fun main()
  [""
  ,example-perfect1().show
  ,example-perfect2().show
  ,example-gtree().show
  ,example-check().show
  ,example-pc2().show
  ,example-choose().show
  //,example-coin().show
  //,example-print1().show
  //,example-print2().show
  ,""
  ].join("\n-----------\n").print
  try {
    example-pc1().show.println // raises exception
  } fn(exn){ println(exn.show) }

