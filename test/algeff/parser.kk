effect parse {
  satisfy<b>( pred: string -> maybe<(b,string)> ) : b;
  choice<b>( p1 :  () -> xparse b, p2 : () -> xparse b ) : b
}

alias xparse = <parse,div>

fun show-parse( xs : list<(a,string)>, f : a -> string ) : string {
  xs.show-list fun(r) {
    r.show-tuple(f,show)
  }
}

fun show-parse-ints( xs : list<(list<int>,string)> ) : string {
  xs.show-parse( fun(is) { is.show-list(show) } )
}

val parse : forall<a,e> (string, () -> <parse,div,cps|e> a) -> <div,cps|e> list<(a,string)>
  = handler(input:string) {
      return x   -> [(x,input)]
      satisfy(pred) -> {
        match(pred(input)) {
          Nothing          -> []
          Just((res,rest)) -> resume(rest,res)
        }
      }
      choice(p1,p2) -> {
        val xs = parse(input,p1) + parse(input,p2)
        xs.flatmap( fun(xr) {
          val (x,rest) = xr
          resume(rest,x)
        })
      }
    }


fun fail() {
  satisfy(fun(input) { Nothing })
}

fun char(p : char -> bool) : parse char {
  satisfy( fun(input) {
    val sc = input.substr(0,1)
    match (sc.list) {
      [c] | p(c) -> Just((c,input.substr(1)))
      _   -> Nothing 
    }
  })
}

fun digit() : parse int {
  int(char(digit?) - '0')
}

fun alpha() : parse int {
  int(char(alpha?) - 'a')
}

fun many(p) {
  manyx([],p)
}

fun manyx(acc,p) {
  choice {
    val x = p()
    manyx(Cons(x,acc),p)
  } { acc.reverse } 
}

fun manyy(p) {
  choice {
    val x = p()
    val xs = manyy(p)
    Cons(x,xs)
  } { [] } 
}

fun test1() {
  parse("12",{ many(digit) }).show-parse-ints.println 
}

fun main() {
  test1()
}