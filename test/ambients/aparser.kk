// --------------------------------------------------------------
// Parser example
// --------------------------------------------------------------

// First defined the `many` effect to describe multiple of results and failure
ambient control choice() : bool
ambient control fail() : a

fun select(xs) {
  match(xs) {
    Nil        -> fail()
    Cons(x,xx) -> if (choice()) then x else select(xx)
  }
}

// Return all solutions
fun solutions(action) {
  with control choice() { resume(True) + (resume(False) : list<_>)}
  with control fail()   { [] }
  [action()]
}


// Return the first solution
fun eager(action) {
  with control choice() {
    match(resume(True)) {
      Nil -> resume(False)
      xs  -> xs
    }
  }
  with control fail()   { [] }
  [action()]
}

// `or` is now defined over `choice` to possibly return multiple results
fun or(p1,p2) {
  if (choice()) then p1() else p2()
}

// Basic combinators
fun many(p : () -> <choice,div|e> a ) : <choice,div|e> list<a> {
  or { many1(p) } { [] }
}

fun many1(p : () -> <choice,div|e> a ) : <choice,div|e> list<a> {
  Cons(p(),many(p))
}


//---------------------------------------------------------------
// Now define the parse effect

ambient fun next() : char

alias parse = <next,choice,fail>
alias parser<a> = () -> parse a

fun parse(p : () -> <parse|e> a, input : string ) : <choice,fail|e> (a,string) {
  var cs := input.list
  val x = with fun next() {
            match(cs) {
              Nil        -> fail()
              Cons(c,cc) -> { cs := cc; c }
            }
          } in p()
  (x,cs.string)
}

fun char(p : char -> bool) : parse char {
  val c = next()
  if (p(c)) then c else fail()
}

// Expression parsers
fun symbol(c : char) : parse () {
  char( fn(d) { c == d })
  ()
}

fun digit() : parse int {
  int(char(digit?) - '0')
}

fun alpha() : parse int {
  int(char(alpha?) - 'a')
}

fun number() {
  many1(digit).foldl(0, fn(n,d) { 10*n + d })
}

fun expr() : <div,parse> int {
  or {
    val i :int = term()
    symbol('+')
    val j = term()
    i+j
  }
  { term() }
}

fun term() {
  or {
    val i : int = factor()
    symbol('*')
    val j = factor()
    i*j
  }
  { factor() }
}

fun factor() {
  or(number) {
    symbol('(')
    val i = expr()
    symbol(')')
    i
  }
}



fun test1() {
  parse( expr, "1+2*3")
}


fun main() {
  solutions(test1).show-parse-int.println
  eager(test1).show-parse-int .println
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
