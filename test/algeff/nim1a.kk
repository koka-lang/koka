// --------------------------------------------------------
// Examples from the paper "Liberating effects with rows and handlers"
// by Daniel Hillerström and Sam Lindley.
// --------------------------------------------------------

// test: recursive aliceTurn and bobTurn should generate right Info records for C# backend

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

effect nim {
  fun move(player:player,sticks:int) : int
}


fun aliceTurn(n) {// : int) : <div,nim> player {
  if (n <= 0) then Bob else bobTurn( n - move(Alice,n) )
}

fun bobTurn(n) { // : int) : <div,nim> player {
  if (n <= 0) then Alice else aliceTurn( n - move(Bob,n) )
}

fun game(n) {
  aliceTurn(n)
}

// --------------------------------------------------------
// Perfect strategy
// --------------------------------------------------------

val perfect = handler {
  return x  -> x
  move(p,n) -> resume(max(1,n%4))
}

fun testPerfect1() {
  perfect{ game(7) }  // alice
}
fun testPerfect2() {
  perfect{ game(12) } // bob
}

fun main() {
  println( testPerfect1().show  + ", " + testPerfect2().show )
}
