effect parse {
  satisfy<b>( pred: string -> maybe<(b,string)> ) : b;
  choice<b>( p1 :  () -> xparse b, p2 : () -> xparse b ) : b
}

alias xparse = <parse,div>

function showP( xs : list<(a,string)>, f : a -> string ) : string {
  xs.showList(fun(r) {
    val (x,rest) = r
    "(" + x.f + "," + rest.show + ")"
  })
}

function showP1( xs : list<(list<int>,string)> ) : string {
  xs.showP( fun(is) { is.showList(show) })
}


val parse = handler(input) {
  return x   -> [(x,input)]
  satisfy(p) -> {
    match(p(input)) {
      Nothing          -> []
      Just((res,rest)) -> resume(rest,res)
    }
  }
  choice(p1,p2) -> {
    val xs = parse(input,p1) + (parse(input,p2) : list<_>)
    xs.concat( fun(xr) {
      val (x,rest) = xr
      resume(rest,x)
    })
  }
}

function fail() {
  satisfy(fun(input) { Nothing })
}

function char(p : char -> bool) : parse char {
  satisfy( fun(input) {
    val sc = input.substr(0,1)
    match (sc.list) {
      [c] | p(c) -> Just((c,input.substr(1)))
      _   -> Nothing 
    }
  })
}

function digit() : parse int {
  int(char(isDigit) - '0')
}

function alpha() : parse int {
  int(char(isAlpha) - 'a')
}

function many(p) {
  manyx([],p)
}

function manyx(acc,p) {
  choice {
    val x = p()
    manyx(Cons(x,acc),p)
  } { acc.reverse } 
}

function manyy(p) {
  choice {
    val x = p()
    val xs = manyy(p)
    Cons(x,xs)
  } { [] } 
}