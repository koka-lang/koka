/* Examples from the paper "Effect handlers in Scope"
   by Nicolas Wu, Tom Schrijvers, and Ralf Hinze
*/

effect nondet {
  control flip() : bool
  except  fail() : a
}

fun knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then []
  else {
    val v = select(vs)
    Cons(v,knapsack(w - v, vs))
  }
}

fun select(xs) {
  match(xs) {
    Nil        -> fail()
    Cons(x,xx) -> if (flip()) then x else select(xx)
  }
}

val solutions = handler {
  return  x      -> [x]
  except  fail() -> []
  control flip() -> resume(True) ++ resume(False)
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
  with {
    return(x)  -> (s,x)
    fun get()  -> s
    fun put(i) -> s := i
  }
  action()
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
  return(x)      -> x
  except  fail() -> fail()
  control flip() -> { incr(); resume(flip()) }
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
// --------------------------------------------------------------

effect cutfail {
  control cutfail() : a
}

fun cut() {
  if (flip()) then () else cutfail()
}

val hcutfail = handler{
  return(x)         -> x
  control cutfail() -> fail()
}

// collect creates thunks of continuations,
// only `cutfail` will `fail` and cut the choices short
fun collect(action) {
  var q := [fail]
  with handler {
    return(x) {
      if (flip()) then x else {
        match(q) {
          Nil -> fail()
          Cons(p,ps) -> { q := ps; p() }
        }
      }
    }
    except fail() {
      match(q) {
        Nil -> fail()
        Cons(p,ps) -> { q := ps; p() }
      }
    }
    control flip() {
      q := Cons({resume(False)},q)
      resume(True)
      // resume(True,{resume(False,p))
    }
  }
  action()
}

fun call(p) {
  hcutfail{ collect(p) }
}

fun once(p) {
  call{ val x = p(); cut(); x }
}

fun test4() {
  solutions{ once{ knapsack(3,[3,2,1]) }}
}

// --------------------------------------------------------------
// Grammars
// --------------------------------------------------------------

effect symbol {
  fun symbol(s : string) : string
}

fun parse(input : string, action : () -> <nondet,symbol|e> a) : <nondet|e> a {
  var cs := input
  handle(action) {
    return (x) { if (cs.is-empty) then x else fail() }
    fun symbol(s) {
      match(cs.starts-with(s)) {
        Nothing   { fail() }
        Just(rest){ cs := rest.string; s }
      }
    }
  }
}

val digits = list(0,9).map(show)

fun digit() : <nondet,symbol> string {
  symbol(select(digits))
}

fun choice(p1,p2) {
  if (flip()) then p1() else p2()
}

fun many(p : () -> <nondet,div|e> a) : <nondet,div|e> list<a> {
  choice{ many1(p) }{ [] }
}

fun many1(p : () -> <nondet,div|e> a) : <nondet,div|e> list<a> {
  Cons(p(),many(p))
}

fun number() {
  val ds = many1(digit)
  parse-int(ds.join).maybe(0)
}

fun expr() : <div,nondet,symbol> int {
  choice {
    val i :int = term()
    symbol("+")
    val j = term()
    i+j
  }
  { term() }
}

fun term() {
  choice {
    val i : int = factor()
    symbol("*")
    val j = factor()
    i*j
  }
  { factor() }
}

fun factor() {
  choice(number) {
    symbol("(")
    val i = expr()
    symbol(")")
    i
  }
}

fun test5() {
  solutions{ parse("2+8*5"){ expr() } }
}


// --------------------------------------------------------------
// Main
// --------------------------------------------------------------

fun main() {
  test1().show.println
  test2().show.println
  test3().show.println
  test4().show.println
}
