//------------------------------------------------
// The choices handler is wrongly typed 
// since it doesn't issue a 'resume' 
//------------------------------------------------

effect nondet {
  choice(xs : list<a>) : a;
}

function fail() {
  choice([])
}

function knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0) then fail()
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
    incr(xs.length)
    // resume(choice(xs))
    choice(xs)
  }
}

function show(sxss : (int,list<list<int>>)) : string {
  "(state=" + sxss.fst.show + ", " + sxss.snd.show + ")"
}
function test2() {
  global(0){ choices{ knapsack(3,[3,2,1]) }}
}