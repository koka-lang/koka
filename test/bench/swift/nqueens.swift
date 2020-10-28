indirect enum List<T> {
  case Nil
  case Cons(T,List<T>)
}

func len<T>( _ xs : List<T> ) -> Int64 {
  var n : Int64 = 0;
  var cur : List<T> = xs
  while true {
    switch(cur) {
      case .Nil: return n
      case let .Cons(_,xx): do {
        n += 1
        cur = xx
      }
    }
  }
}

func safe( _ queen : Int64, _ xs : List<Int64> ) -> Bool {
  var cur : List<Int64> = xs
  var diag : Int64 = 1
  while true {
    switch(cur) {
      case .Nil: return true
      case let .Cons(q,xx): do {
        if (queen == q || queen == (q + diag) || queen == (q - diag)) {
          return false
        }
        diag += 1
        cur = xx
      }
    }
  }
}

// todo: use while?
func appendSafe( _ k : Int64, _ soln : List<Int64>, _ solns : List<List<Int64>> ) -> List<List<Int64>> {
  var acc = solns
  var n = k
  while(n > 0) {
    if (safe(n,soln)) {
      acc = .Cons(.Cons(n,soln),acc)
    }
    n -= 1;
  }
  return acc
}


func extend( _ n : Int64, _ solns : List<List<Int64>> ) -> List<List<Int64>> {
  var acc : List<List<Int64>> = .Nil
  var cur = solns
  while(true) {
    switch(cur) {
      case .Nil: return acc
      case let .Cons(soln,rest): do {
        acc = appendSafe(n,soln,acc)
        cur = rest
      }
    }
  }
}

func findSolutions(_ n : Int64 ) -> List<List<Int64>> {
  var k = 0
  var acc : List<List<Int64>> = .Cons(.Nil,.Nil)
  while( k < n ) {
    acc = extend(n,acc)
    k += 1
  }
  return acc
}

func nqueens(_ n : Int64) -> Int64 {
  return len(findSolutions(n))
}

print(nqueens(13))

/*
len xs
  = len' xs 0

len' xs acc
  = case xs of
      Nil -> acc
      Cons _ t -> len' t $! (acc+1)

safe queen diag xs
  = case xs of
      Nil      -> True
      Cons q t -> queen /= q && queen /= q + diag && queen /= q - diag && safe queen (diag + 1) t

appendSafe k soln solns
  = if (k <= 0)
     then solns
     else if safe k 1 soln
           then appendSafe (k-1) soln (Cons (Cons k soln) solns)
           else appendSafe (k-1) soln solns


extend n acc solns
  = case solns of
      Nil            -> acc
      Cons soln rest -> extend n (appendSafe n soln acc) rest

find_solutions n k
  = if k == 0
     then Cons Nil Nil
     else extend n Nil (find_solutions n (k-1))

-- fst_solution n = head (find_solutions n n)

queens n
  = len (find_solutions n n)

main
  = print (queens 13)


enum Color {
  case Red
  case Black
}

indirect enum Tree {
  case Leaf
  case Node(Color, Tree, UInt64, Bool, Tree)
}

func balance1(_ kv : UInt64, _ vv : Bool, _ t : Tree, _ n : Tree) -> Tree {
  switch n {
  case let .Node(_, .Node(.Red, l, kx, vx, r1), ky, vy, r2):
    return .Node(.Red, .Node(.Black, l, kx, vx, r1), ky, vy, .Node(.Black, r2, kv, vv, t))
  case let .Node(_, l1, ky, vy, .Node(.Red, l2, kx, vx, r)):
    return .Node(.Red, .Node(.Black, l1, ky, vy, l2), kx, vx, .Node(.Black, r, kv, vv, t))
  case let .Node(_, l, ky, vy, r):
    return .Node(.Black, .Node(.Red, l, ky, vy, r), kv, vv, t)
  default:
    return .Leaf
  }
}

func balance2(_ t : Tree, _ kv : UInt64, _ vv : Bool, _ n : Tree) -> Tree {
  switch n {
  case let .Node(_, .Node(.Red, l, kx1, vx1, r1), ky, vy, r2):
    return .Node(.Red, .Node(.Black, t, kv, vv, l), kx1, vx1, .Node(.Black, r1, ky, vy, r2))
  case let .Node(_, l1, ky, vy, .Node(.Red, l2, kx2, vx2, r2)):
    return .Node(.Red, .Node(.Black, t, kv, vv, l1), ky, vy, .Node(.Black, l2, kx2, vx2, r2))
  case let .Node (_, l, ky, vy, r):
    return .Node(.Black, t, kv, vv, .Node(.Red, l, ky, vy, r))
  default:
    return .Leaf
  }
}

func is_red (_ t : Tree) -> Bool {
  switch t {
  case .Node(.Red, _, _, _, _):
    return true
  default:
    return false
  }
}

func ins(_ t : Tree, _ kx : UInt64, _ vx : Bool) -> Tree {
  switch t {
  case .Leaf:
    return .Node(.Red, .Leaf, kx, vx, .Leaf)
  case let .Node(.Red, a, ky, vy, b):
    if kx < ky {
      return .Node(.Red, ins(a, kx, vx), ky, vy, b)
    } else if ky == kx {
      return .Node(.Red, a, kx, vx, b)
    } else {
      return .Node(.Red, a, ky, vy, ins(b, kx, vx))
    }
  case let .Node(.Black, a, ky, vy, b):
   if kx < ky {
    if is_red(a) {
      return balance1(ky, vy, b, ins(a, kx, vx))
    } else {
      return .Node(.Black, ins(a, kx, vx), ky, vy, b)
    }
   } else if kx == ky {
     return .Node(.Black, a, kx, vx, b)
   } else {
     if is_red(b) {
       return balance2(a, ky, vy, ins(b, kx, vx))
     } else {
       return .Node(.Black, a, ky, vy, ins(b, kx, vx))
     }
   }
  }
}

func set_black (_ n : Tree) -> Tree {
  switch n {
  case let .Node (_, l, k, v, r):
    return .Node (.Black, l, k, v, r)
  default:
    return n
  }
}

func insert (_ t : Tree, _ k : UInt64, _ v : Bool) -> Tree {
 if is_red(t) {
   return set_black(ins(t, k, v))
 } else {
   return ins(t, k, v)
 }
}

func fold (_ f : (_ k : UInt64, _ v : Bool, _ d : UInt64) -> UInt64, _ n : Tree, _ d : UInt64) -> UInt64 {
  switch n {
  case .Leaf:
    return d
  case let .Node(_, l, k, v, r):
    return fold(f, r, f(k, v, fold(f, l, d)))
  }
}

func mk_map (_ n : UInt64) -> Tree {
  var i = n
  var m : Tree = .Leaf
  while i > 0 {
    i = i - 1
    m = insert(m, i, (i%10 == 0))
  }
  return m
}

func aux (_ k : UInt64, _ v : Bool, _ r : UInt64) -> UInt64 {
  if v {
    return r + 1
  } else {
    return r
  }
}

var num: UInt64? = 4200000
if CommandLine.arguments.count >= 2 {
  num = UInt64(CommandLine.arguments[1])
}
let m = mk_map(num!)
let v = fold(aux, m, 0)
print(v)
*/
