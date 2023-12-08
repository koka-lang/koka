
pub value struct linear-set<v>
  pub list: list<v>
  eq: some<e> (v, v) -> e bool

pub fun (+)(l: linear-set<v>, a: v) : e linear-set<v>
  if (l.list.any(fn(x) (l.eq)(x, a))) then l else l(list=Cons(a, l.list))

pub fun (+)(l1: linear-set<v>, l2: linear-set<v>) : e linear-set<v>
  l2.list.foldl(l1, fn(acc, x) acc + x)

pub fun foo0(l1: linear-set<int>, l2: linear-set<int>): e linear-set<int>
  [l2].foldl(l1, fn(acc,x) acc + x)

pub fun foo1(l1: linear-set<int>, l2: linear-set<int>): e linear-set<int>
  [l2].foldl(l1, fn(acc : linear-set<int>, x : linear-set<int>) acc + x)

pub fun foo2(l1: linear-set<int>, l2: linear-set<int>): e linear-set<int>
  [l2].foldl(l1, (fn(acc,x) acc + x) : (linear-set<int>,linear-set<int>) -> _e linear-set<int>)
