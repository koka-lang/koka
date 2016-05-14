// --------------------------------------------------------------
// Examples from the paper "Effect handlers in Scope"
// by Nicolas Wu, Tom Schrijvers, and Ralf Hinze
//
// but now using a choice operator that chooses directly from a list.
// --------------------------------------------------------------
effect nondet {
  choice(xs : list<a>) : a;
}

function fail() {
  choice([])
}

function knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then []
  else {
    val v = choice(vs)
    Cons(v,knapsack(w - v, vs))
  }
}

val solutions = handler {
  return x   -> [x]
  choice(xs) -> xs.concat(resume)
}

function show( xss : list<list<int>> ) : string {
  xss.showList(fun(xs) { xs.showList(core/show) } )
}

function test1() {
  solutions{ knapsack(3,[3,2,1]) }
}

// --------------------------------------------------------------
// with local and global state
// --------------------------------------------------------------

effect state<s> {
  get()    : s;
  put(i:s) : ()
}

val state = handler(i) {
  return x -> (i,x)
  get()    -> resume(i,i)
  put(j)   -> resume(j,()) 
}

function incr(i = 1) : state<int> () {
  put(get()+i)
}

function local(s,action) {
  solutions{ state(s,action) }
}

function global(s,action) {
  state(s){ solutions(action) }
}

val choices = handler {
  return x   -> x
  choice(xs) -> {
    val y = choice(xs)
    incr()
    resume(y)
  }
}

function show(x : (int,list<list<int>>)) : string {
  "(state=" + x.fst.show + ", " + x.snd.show + ")"
}

function show(xs : list<(int,list<int>)>) : string {
  xs.showList fun(x) {
    "(state=" + x.fst.show + ", " + x.snd.show + ")"
  }
}

function test2() {
  global(0){ choices{ knapsack(3,[3,2,1]) }}
}

function test3() {
  local(0){ choices{ knapsack(3,[3,2,1]) }}
}

// --------------------------------------------------------------
// Cut
// We cannot implement this orthogally to choices :-( (I think)
// --------------------------------------------------------------

// todo: should be the standard library
function untilJust( xs : list<a>, f : a -> e maybe<b> ) : e maybe<b> {
  match(xs) {
    Nil -> Nothing
    Cons(x,xx) -> {
      match(f(x)) {
        Nothing -> xx.untilJust(f)
        just    -> just
      }
    }
  }
}

val solution = handler {
  return x   -> Just(x)
  choice(xs) -> xs.untilJust( fun(x) { resume(x) })
}

function show(m : maybe<list<int>> ) : string {
  match(m) {
    Nothing -> "none"
    Just(xs)-> "one: " + xs.show
  }
}

function test4() {
  solution{ knapsack(3,[3,2,1])}
}

function main() {
  [test1().show
  ,test2().show
  ,test3().show
  ,test4().show].join("\n").println
}