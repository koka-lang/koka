// B-trees with minimum degree t = 2, also known as 2-3-4-trees
// Introduction to Algorithms, Cormen et al. Chapter 18

// We make all nodes and leaves the same size, by 'padding' them with ints
// but keep the two categories seperate.

module b-trees-pad-separate-fast-path

import base

alias elem = int

type tree {
  Root0
  Node1(a : tree, b : elem, c : tree, d : int, e : int, f : int, g : int)
  Node2(a : tree, b : elem, c : tree, d : elem, e : tree, f : int, g : int)
  Node3(a : tree, b : elem, c : tree, d : elem, e : tree, f : elem, g : tree)
  Leaf1(a : elem, b : int, c : int)
  Leaf2(a : elem, b : elem, c : int)
  Leaf3(a : elem, b : elem, c : elem)
}

// We keep the smallest element that is larger than k and compare it with
// k at the end. This reduces the number of comparisons by a factor of 2.
fun member2(tree, k, smallestLarger) {
  match(tree) {
    Root0
      -> wont-happen()
    Node1(a, b, c, _, _, _, _)
      -> if(k <= b) then member2(a, k, b)
      else member2(c, k, smallestLarger)
    Node2(a, b, c, d, e, _, _)
      -> if(k <= b) then member2(a, k, b)
      elif (k <= d) then member2(c, k, d)
      else member2(e, k, smallestLarger)
    Node3(a, b, c, d, e, f, g)
      -> if(k <= b) then member2(a, k, b)
      elif (k <= d) then member2(c, k, d)
      elif (k <= f) then member2(e, k, f)
      else member2(g, k, smallestLarger)
    Leaf1(a, _, _)
      -> a == k || smallestLarger == k
    Leaf2(a, b, _)
      -> a == k || b == k || smallestLarger == k
    Leaf3(a, b, c)
      -> a == k || b == k || c == k || smallestLarger == k
  }
}

fun member(tree, k) {
  match(tree) {
    Root0
      -> False
    Node1(a, b, c, _, _, _, _)
      -> if(k <= b) then member2(a, k, b)
      else member(c, k)
    Node2(a, b, c, d, e, _, _)
      -> if(k <= b) then member2(a, k, b)
      elif (k <= d) then member2(c, k, d)
      else member(e, k)
    Node3(a, b, c, d, e, f, g)
      -> if(k <= b) then member2(a, k, b)
      elif (k <= d) then member2(c, k, d)
      elif (k <= f) then member2(e, k, f)
      else member(g, k)
    Leaf1(a, _, _)
      -> a == k
    Leaf2(a, b, _)
      -> a == k || b == k
    Leaf3(a, b, c)
      -> a == k || b == k || c == k
  }
}

type splitted {
  NoSplit
  Split(l : tree, a : elem, r : tree)
}

inline fun try-split(tree) {
  match(tree) {
    Node3(a, b, c, d, e, f, g) -> Split(Node1(a, b, c, 0, 0, 0, 0), d, Node1(e, f, g, 0, 0, 0, 0))
    Leaf3(a, b, c) -> Split(Leaf1(a, 0, 0), b, Leaf1(c, 0, 0))
    _ -> NoSplit
  }
}

// We keep the smallest element that is larger than k and compare it with
// k at the end. This reduces the number of comparisons by a factor of 2.
fun insert2(tree, k, smallestLarger) {
  match(tree) {
    Node1(a, b, c, n1, n2, n3, n4)
      -> if(k <= b) then match(try-split(a)) {
        NoSplit -> Node1(insert2(a, k, b), b, c, n1, n2, n3, n4)
        Split(x, y, z)
          -> if(k <= y) then Node2(insert2(x, k, y), y, z, b, c, n3, n4)
          else Node2(x, y, insert2(z, k, b), b, c, n3, n4)
      } else match(try-split(c)) {
        NoSplit -> Node1(a, b, insert2(c, k, smallestLarger), n1, n2, n3, n4)
        Split(x, y, z)
          -> if(k <= y) then Node2(a, b, insert2(x, k, y), y, z, n3, n4)
          else Node2(a, b, x, y, insert2(z, k, smallestLarger), n3, n4)
      } 
    Node2(a, b, c, d, e, n1, n2)
      -> if(k <= b) then match(try-split(a)) {
        NoSplit -> Node2(insert2(a, k, b), b, c, d, e, n1, n2)
        Split(x, y, z)
          -> if(k <= y) then Node3(insert2(x, k, y), y, z, b, c, d, e)
          else Node3(x, y, insert2(z, k, b), b, c, d, e)
      } elif(k <= d) then match(try-split(c)) {
        NoSplit -> Node2(a, b, insert2(c, k, d), d, e, n1, n2)
        Split(x, y, z)
          -> if(k <= y) then Node3(a, b, insert2(x, k, y), y, z, d, e)
          else Node3(a, b, x, y, insert2(z, k, d), d, e)
      } else match(try-split(e)) {
        NoSplit -> Node2(a, b, c, d, insert2(e, k, smallestLarger), n1, n2)
        Split(x, y, z)
          -> if(k <= y) then Node3(a, b, c, d, insert2(x, k, y), y, z)
          else Node3(a, b, c, d, x, y, insert2(z, k, smallestLarger))
      }
    Leaf1(a, n1, n2)
      -> if(a == k || smallestLarger == k) then Leaf1(a, n1, n2)
         elif(k < a) then Leaf2(k, a, n2) else Leaf2(a, k, n2)
    Leaf2(a, b, n1)
      -> if(a == k || b == k || smallestLarger == k) then Leaf2(a, b, n1)
         elif(k < a) then Leaf3(k, a, b)
         elif(k < b) then Leaf3(a, k, b) else Leaf3(a, b, k)
    Root0 -> wont-happen()
    Node3(_, _, _, _, _, _, _) -> wont-happen()
    Leaf3(_, _, _) -> wont-happen()
  }
}

fun insert(tree, k) {
  match(tree) {
    Node1(a, b, c, n1, n2, n3, n4)
      -> if(k <= b) then match(try-split(a)) {
        NoSplit -> Node1(insert2(a, k, b), b, c, n1, n2, n3, n4)
        Split(x, y, z)
          -> if(k <= y) then Node2(insert2(x, k, y), y, z, b, c, n3, n4)
          else Node2(x, y, insert2(z, k, b), b, c, n3, n4)
      }
      else match(try-split(c)) {
          NoSplit -> Node1(a, b, insert(c, k), n1, n2, n3, n4)
          Split(x, y, z)
            -> if(k <= y) then Node2(a, b, insert2(x, k, y), y, z, n3, n4)
            else Node2(a, b, x, y, insert(z, k), n3, n4)
      } 
    Node2(a, b, c, d, e, n1, n2)
      -> if(k <= b) then match(try-split(a)) {
        NoSplit -> Node2(insert2(a, k, b), b, c, d, e, n1, n2)
        Split(x, y, z)
          -> if(k <= y) then Node3(insert2(x, k, y), y, z, b, c, d, e)
          else Node3(x, y, insert2(z, k, b), b, c, d, e)
      }
      elif(k <= d) then match(try-split(c)) {
        NoSplit -> Node2(a, b, insert2(c, k, d), d, e, n1, n2)
        Split(x, y, z)
          -> if(k <= y) then Node3(a, b, insert2(x, k, y), y, z, d, e)
          else Node3(a, b, x, y, insert2(z, k, d), d, e)
      }
      else match(try-split(e)) {
          NoSplit -> Node2(a, b, c, d, insert(e, k), n1, n2)
          Split(x, y, z)
            -> if(k <= y) then Node3(a, b, c, d, insert2(x, k, y), y, z)
            else Node3(a, b, c, d, x, y, insert(z, k))
      }
    Leaf1(a, n1, n2)
      -> if(k <= a) then if(a == k) then Leaf1(a, n1, n2) else Leaf2(k, a, n2)
      else Leaf2(a, k, n2)
    Leaf2(a, b, n1)
      -> if(k <= a) then if(a == k) then Leaf2(a, b, n1) else Leaf3(k, a, b)
      elif (k <= b) then if(b == k) then Leaf2(a, b, n1) else Leaf3(a, k, b)
      else Leaf3(a, b, k)

    // Can only happen at the root:
    Root0 -> Leaf1(k, 0, 0)
    Leaf3(a, b, c) -> insert(Node1(Leaf1(a, 0, 0), b, Leaf1(c, 0, 0), 0, 0, 0, 0), k)
    Node3(a, b, c, d, f, g, h) -> insert(Node1(Node1(a, b, c, 0, 0, 0, 0), d, Node1(f, g, h, 0, 0, 0, 0), 0, 0, 0, 0), k)
  }
}

// Benchmark

fun from-list(xs) {
  fun go(xs, t) {
    match(xs) {
      Nil -> t
      Cons(x, xx) -> go(xx, insert(t, x))
    }
  }
  go(xs, Root0)
}

public fun count-b-trees-pad-separate-fast-path(start : int, end : int, xs : list<int>) {
  val t = from-list(xs)
  fun go(i : int, acc : int) {
    if(i <= end) {
      match(member(t, i)) {
        True -> go(i+1, acc+1)
        False -> go(i+1, acc)
      }
    } else acc
  }
  go(start, 0)
}