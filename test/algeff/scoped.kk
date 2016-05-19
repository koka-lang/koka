// --------------------------------------------------------------
// Examples from the paper "Effect handlers in Scope"
// by Nicolas Wu, Tom Schrijvers, and Ralf Hinze
// --------------------------------------------------------------

effect nondet {
  flip() : bool;
  fail() : a
}

function knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then []
  else {
    val v = select(vs)
    Cons(v,knapsack(w - v, vs))
  }
}

function select(xs) {
  match(xs) {
    Nil        -> fail()
    Cons(x,xx) -> if (flip()) then x else select(xx) 
  }
}

val solutions = handler {
  return x -> [x]
  fail()   -> []
  flip()   -> resume(True) + resume(False)
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
  return x -> x
  fail()   -> fail()
  flip()   -> { incr(); resume(flip()) }
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
// --------------------------------------------------------------

effect cutfail {
  cutfail() : a
}

function cut() {
  if (flip()) then () else cutfail()
}

val hcutfail = handler{
  return x  -> x
  cutfail() -> fail()
}

// collect creates thunks of continuations,
// only `cutfail` will `fail` and cut the choices short
val collect = handler(q=fail) {
  return x -> if (flip()) then x else q()
  fail     -> q()
  flip()   -> resume({resume(q,False)}, True)  
}

function call(p) { 
  hcutfail{ collect(p) }
}

function once(p) {
  call{ val x = p(); cut() x }
}

function test4() {
  solutions{ once{ knapsack(3,[3,2,1]) }}
}

// --------------------------------------------------------------
// Grammars
// --------------------------------------------------------------

effect symbol {
  symbol(c : char) : char
}

val parse = handler(cs) {
  return x -> if (cs.isNil) then x else fail()
  symbol(s)-> match(cs) {
                Cons(c,cc) | c == s -> resume(cc,s)
                _ -> fail()
              }              
}


function digit() : <nondet,symbol> char {
  symbol(select(list('0','9')))
}

function choice(p1,p2) {
  if (flip()) then p1() else p2()
}

function many(p) {
  choice{ many1(p) }{ [] }
}

function many1(p) {
  Cons(p(),many(p))
}

function number() {
  val ds = many1(digit)
  parseIntDefault(ds.string,0)
}

function expr() : <div,nondet,symbol> int {
  choice { 
    val i :int = term()
    symbol('+')
    val j = term()
    i+j
  }
  { term() }
}

function term() {
  choice {
    val i : int = factor()
    symbol('*')
    val j = factor()
    i*j
  }
  { factor() }
}

function factor() {
  choice(number) {
    symbol('(')
    val i = expr()
    symbol(')')
    i
  }
}

function test5() {
  solutions{ parse("2+8*5".list){ expr() } }
}


// --------------------------------------------------------------
// Scoped handlers
// --------------------------------------------------------------
/*
effect call {
  bcall() : ();
  ecall() : ()
}

function call2(p) {
  bcall()
  val x = p()
  ecall()
  x
}

function hbcall(action : () -> <exn,div,cutfail,nondet> _) {
  handle(action) {
    return x -> x
    bcall()  -> hbcall{ call{ hecall{ uncps{ resume(()) } } }}
    ecall()  -> error("mismatched ecall")
  } 
}

function hecall(action : () -> <pure,cutfail,nondet> _) { //: <pure,cutfail,nondet> (() -> <pure,cutfail,nondet,cps> a) {
  handle(action) {
    return x -> fun(_){ x }
    ecall()  -> resume
    bcall()  -> hecall{ call{ hecall{ uncps{ resume(()) } } }}
  }
}

function runCut(p) {
  call{ hbcall(p) }
}

function expr3() {
  val i = term()
  call2{ 
    choice {
      symbol('+')
      val j = expr3()
      i+j    
    }{ i }
  }
}
function test6() {
  ()//solutions{ runCut{ parse("1".list,expr3) } }
}
*/

// --------------------------------------------------------------
// Main
// --------------------------------------------------------------

function main() {
  [test1().show
  ,test2().show
  ,test3().show
  ,test4().show
  ,test5().show
  ].join("\n").println
}
