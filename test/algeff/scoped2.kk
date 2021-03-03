// --------------------------------------------------------------
// Examples from the paper "Effect handlers in Scope"
// by Nicolas Wu, Tom Schrijvers, and Ralf Hinze
//
// but now using a choice operator that chooses directly from a list.
// --------------------------------------------------------------
effect nondet {
  control choice(xs : list<a>) : a
}

fun fail() {
  choice([])
}

fun knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then []
  else {
    val v = choice(vs)
    Cons(v,knapsack(w - v, vs))
  }
}

val solutions = handler {
  return x   -> [x]
  control choice(xs) -> xs.flatmap(fn(x){ resume(x) })
}

fun show( xss : list<list<int>> ) : string {
  xss.show-list(fn(xs) { xs.show-list(core/show) } )
}

fun test1() {
  solutions{ knapsack(3,[3,2,1]) }
}

// --------------------------------------------------------------
// with local and global state
// --------------------------------------------------------------

effect state<s> {
  fun get()    : s
  fun put(i:s) : ()
}

fun state(init,action) {
  var s := init
  handle(action) {
    return (x) -> (s,x)
    fun get()  -> s
    fun put(i) -> s := i
  }
}

fun incr(i = 1) : state<int> () {
  put(get()+i)
}

fun local(s,action) {
  solutions{ state(s,action) }
}

fun global(s,action) {
  state(s){ solutions(action) }
}

val choices = handler {
  return x   -> x
  control choice(xs) -> {
    val y = choice(xs)
    incr()
    resume(y)
  }
}

fun show(x : (int,list<list<int>>)) : string {
  "(state=" ++ x.fst.show ++ ", " ++ x.snd.show ++ ")"
}

fun show(xs : list<(int,list<int>)>) : string {
  xs.show-list fn(x) {
    "(state=" ++ x.fst.show ++ ", " ++ x.snd.show ++ ")"
  }
}

fun test2() {
  global(0){ choices{ knapsack(3,[3,2,1]) }}
}

fun test3() {
  local(0){ choices{ knapsack(3,[3,2,1]) }}
}

// --------------------------------------------------------------
// Cut
// We cannot implement this orthogally to choices :-( (I think)
// --------------------------------------------------------------

// todo: should be the standard library
fun untilJust( xs : list<a>, f : a -> e maybe<b> ) : e maybe<b> {
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
  return (x){ Just(x) }
  rcontrol choice(xs) { xs.untilJust( fn(x) { rcontext.resume(x) }) }
}

fun show(m : maybe<list<int>> ) : string {
  match(m) {
    Nothing -> "none"
    Just(xs)-> "one: " ++ xs.show
  }
}

fun test4() {
  solution{ knapsack(3,[3,2,1])}
}

fun main() {
  test1().show.println
  test2().show.println
  test3().show.println
  test4().show.println
}
