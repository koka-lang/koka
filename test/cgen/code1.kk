
pub fun f(x : int) : div int {
  if (x==0) then 1 else g(x) + 1;
}
fun g(x : int) : div int {
  g(f(x - 1));
}

private fun const(x,y) {
  x
}

fun noinline apply(f,x) {
  f(x)
}

fun incr(x : int) : int {
  x + 1
}

fun fib(n:int) : int {
  if (n==0) then 1 else fib(unsafe-decreasing(n - 1))
}

val incrx = (if (fib(10)==1) then incr else id)

fun val-apply(x : int) : int {
  incrx(x)
}

fun eta-test1() {
  val-apply(id(id)(1))
}

fun val-apply2(x:int ) : int {
  code1/apply(val-apply,1)
}

fun box-test1( x : int ) : int {
  code1/apply(incr,x)
}

fun box-test2( xs : list<int> ) : list<int> {
  xs.map(incr)
}

fun box-test3( xs : list<int> ) : int {
  xs.map(Just).head(Nothing).default(42)
}

fun box-test4( xs : list<int> ) : int {
  val x = xs.map(Just)
  drop(xs)
  match(x) {
    Cons(Just(x)) -> x
    Cons(Just(y1) as x1,Cons(Just(y2) as x3, _ as x4) as x2) 
      -> dup(y); drop(x); y
         
         drop-match(x, 
           {drop-match(x1, {}, {dup(y1)});
            drop-match(x2, { drop-match(x3, {}, {dup(y2)} ); drop(x4) }, { dup(y2) }) 
           },
           {dup(y1);dup(y2)})
         
    Cons(y,xx) ->  drop-match(x,{},{dup(y);dup(xx)})
         
      
       
    _ -> 42
  }
}

fun call-test4( i : int, x : int ) : maybe<int> {
  Just((if (i==42) then id else inc)(x))
}
fun mapx( xs, f ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
  }
}

fun map-acc( xs, f, acc ) {
  match(xs) {
    Nil -> acc.reverse
    Cons(x,xx) -> xx.map-acc(f,Cons(f(x),acc))
  }
}
