// --------------------------------------------------------------
// Parser example
// --------------------------------------------------------------

// First defined the `many` effect to describe multiple of results and failure
effect many {
  control flip() : bool
  control fail() : a
}

fun select(xs) {
  match(xs) {
    Nil        -> fail()
    Cons(x,xx) -> if (flip()) then x else select(xx) 
  }
}

// Return all solutions 
val solutions = handler {
  return  x       -> [x]
  control fail()  -> []
  control flip()  -> resume(True) ++ resume(False)
}


// Return the first solution
val eager = handler {
  return x       -> [x]
  control fail() -> []
  control flip() -> match(resume(True)) {
    Nil -> resume(False)
    xs  -> xs
  }
}

// Choice is now defined over `flip` to possibly return multiple results
fun choice(p1,p2) {
  if (flip()) then p1() else p2()
}

// Basic combinators
fun many(p : () -> <many,div|e> a ) : <div,many|e> list<a> {
  choice { many1(p) } { [] } 
}

fun many1(p : () -> <many,div|e> a ) : <div,many|e> list<a> {
  Cons(p(),many(p))
}


//---------------------------------------------------------------
// Now define the parse effect

effect parse {
  fun satisfy<b>( pred: string -> maybe<(b,string)> ) : b
}

alias mparse = <parse,many>

// The parse handler uses the `many` effect to return results
fun parse(input0 : string, action : () -> <parse,many|e> a) : <many|e> (a,string) {
  var input := input0
  with return(x){ (x,input) }
  with fun satisfy(pred){
    match(pred(input)) {
      Nothing          { fail() }
      Just((res,rest)) { input := rest; res }
    }
  }
  action()      
}


fun char(p : char -> bool) : parse char {
  satisfy( fn(input) {
    val sc = input.head
    match (sc.list) {
      [c] | p(c) -> Just((c,input.tail))
      _   -> Nothing 
    }
  })
}

// Expression parsers
fun symbol(c : char) : parse () {
  char( fn(d) { c == d })
  ()
}

fun digit() : parse int {
  int(char(is-digit) - '0')
}

fun alpha() : parse int {
  int(char(is-alpha) - 'a')
}

fun number() {
  many1(digit).foldl(0, fn(n,d) { 10*n + d })
}

fun expr() : <div,mparse> int {
  choice { 
    val i :int = term()
    symbol('+')
    val j = term()
    i+j
  }
  { term() }
}

fun term() {
  choice {
    val i : int = factor()
    symbol('*')
    val j = factor()
    i*j
  }
  { factor() }
}

fun factor() {
  choice(number) {
    symbol('(')
    val i = expr()
    symbol(')')
    i
  }
}


// Testing
// Parse for all solutions
fun mparse(p,input) {
  solutions{ parse(input,p) }
}

// Parse eagerly
fun eparse(p,input) {
  eager{ parse(input,p) }
}

fun test1() {
  mparse( expr, "1+2*3").show-parse-int.println 
}

fun main() {
  test1()
}

// Some show functions
fun show-parse( xs : list<(a,string)>, f : a -> string ) : string {
  xs.show-list fn(r) {
    r.show-tuple(f,show)
  }
}

fun show-parse-int( xs : list<(int,string)> ) : string {
  xs.show-parse(show)
}

fun show-parse-ints( xs : list<(list<int>,string)> ) : string {
  xs.show-parse( fn(is) { is.show-list(show) } )
}
