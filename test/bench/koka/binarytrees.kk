import std/num/int32

type tree {
  Leaf
  Node(l : tree, r : tree)
}

// This Function has an extra argument to suppress the
// common sub-expression elimination optimization
// (comment from Haskell version, does this apply here?)
fun make'(n : int32, d : int32) : div tree {
  if(d.is-zero) then Node(Leaf, Leaf)
  else Node(make'(n, d.dec), make'(n.inc, d.dec))
}

// build a tree
fun make(d : int32) : div tree {
  // make''(d, Down, Root)
  make'(d, d)
}

fun checkTail(t : tree, a : int32) : int32 {
  match(t) {
    Leaf -> a
    Node(l, r) -> checkTail(l, checkTail(r, a + 1.int32))
  }
}

fun check(t : tree) : int32 {
  checkTail(t, 0.int32)
}

val minN = 4

fun out(s : string, n : int, t : int32) : console () {
  println(s ++ " of depth " ++ n.show ++ "\t check: " ++ t.show)
}

// allocate and check lots of trees
fun sumT(d : int32, i : int32, t : int32) : div int32 {
  if(i.is-zero) then t
  else {
    val a = check(make(d))
    sumT(d, i.dec, t + a)
  }
}

// generate many trees
fun depth(d : int, m : int) : div list<(int, int, int32)> {
  if(d <= m) then {
    val n = 2.pow(m - d + minN)
    Cons((n, d, sumT(d.int32, n.int32, 0.int32)), depth(d + 2, m))
  }
  else Nil
}

fun main() {
  val n = 5
  val maxN = max(minN + 2, n)
  val stretchN = maxN + 1

  // Stretch memory tree
  val c = check(make(stretchN.int32))
  out("stretch tree", stretchN, c)

  // Allocate a long lived tree
  val long = make(maxN.int32)

  // allocate, walk, and deallocate many bottom-up binary trees
  val vs = depth(minN, maxN)
  vs.foreach(fn((m, d, i)) { out(m.show ++ "\t trees", d, i) })

  out("long lived tree", maxN, check(long))
}