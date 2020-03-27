module ev3
import std/num/int32

extern include {
  js file "yld.js"
}


// -------------------------------------------
// Internal types
// -------------------------------------------

struct marker<e::E,a>(m:int32)

public type htag<h::(E,V)->V> {
  Unsafe-htag(tag:string)
}

type ev<h> {
  con Ev<e,r>(:htag<h>, :marker<e,r>, :h<e,r>, :evv<e>)
}

type evv<e::E>;


// -------------------------------------------
// Internal Markers
// -------------------------------------------

extern fresh-marker-int() : int32 {
  js inline "_marker_unique++"
}

fun fresh-marker() : marker<e,r> {
  Marker(fresh-marker-int())
}

fun (==)(m1 : marker<e1,a1>, m2: marker<e2,a2>) : bool {
  (m1.m == m2.m)
}

// -------------------------------------------
// Internal Evidence vectors
// -------------------------------------------

public extern inline evv-total() : evv<<>> {
  js inline "[]"
}

extern inline evv-equal(w1 : evv<e>, w2 : evv<e> ) : bool {
  js inline "(#1===#2)"
}

extern evv-insert( evv : evv<e1>, ev : ev<h> ) : evv<e2> {
  js "evv_insert_ev"
}

public extern ievv-lookup( htag : htag<h> ) : ev<h> {
  js inline "evv_lookup(_context.evv,#1)"
}

public extern inline ievv-select-at( i : int ) : ev<h> {
  js inline "(_context.evv)[#1]"
}

public extern inline ievv-select( i : int, htag : htag<h> ) : ev<h> {
  js inline "(#1 >= 0 ? _context.evv[#1] : evv_lookup(_context.evv,#2))"
}


extern evv-current() : evv<e> {
  js inline "_context.evv"
}

extern evv-set(w : evv<e>) : () {
  js inline "(_context.evv = #1)"
}

extern under( w : evv<e1>, action : () -> yld<e1,a> ) : yld<e,a> {   // e1 <= e
  js inline "(function(){ const w0 = _context.evv; _context.evv = #1; const res = (#2)(); _context.evv = w0; return res; })()"
}

fun under( w : evv<e1>, action : (a) -> yld<e1,b>, x : a ) : yld<e,b> {
  under(w){ action(x) }
}

fun under( w : evv<e1>, action : (a,b) -> yld<e1,c>, x : a, y : b ) : yld<e,c> {
  under(w){ action(x,y) }
}

// -------------------------------------------
// Yield monad
// -------------------------------------------

abstract rectype yld<e::E,a> {
  Pure(x:a)
  Yield<b,e1,r>(marker:marker<e1,r>, clause:(k:((b) -> yld<e1,r>)) -> yld<e1,r>, cont:(b -> yld<e,a>))
}


public fun yld-pure(x : a) : yld<e,a> {
  Pure(x)
}

fun kcompose(f : (b -> yld<e,c>), g : a -> yld<e,b>, x : a ) : yld<e,c> {
  match(g(x)) {
    Pure(y) -> f(y)
    Yield(m,clause,cont) -> Yield(m,clause,fun(y){ kcompose(unsafe-decreasing(f),cont,y) })
  }
}

public fun yld-bind(yld : yld<e,a>, next : (a -> yld<e,b>)) : yld<e,b> {
  match(yld) {
    Pure(x) -> next(x)
    Yield(m,clause,cont) -> Yield(m,clause,fun(x){ kcompose(next,cont,x) })
  }
}

public fun yld-run(yld : () -> yld<<>,a>) : exn a {
  evv-set(evv-total())
  match(yld()) {
    Pure(x) -> x
  }
}
extern inline unsafe-coerce(x:a) : b { inline "#1" }

fun prompt( w1:evv<e1>, m : marker<e1,r>, res : yld<e2,r> ) : yld<e1,r> {
  match(res) {
    Pure(x) -> Pure(x)
    Yield(n,clause,cont) {
      val deep = fun(x){ prompt(w1,unsafe-decreasing(m),cont(x)) }
      if (m == n) then unsafe-coerce(clause)(fun(x){ guard(w1,deep,x) }) else Yield(n,clause,deep)
    }
  }
}

fun guard( w1 : evv<e>, cont : a -> yld<e,b>, x:a ) : e yld<e,b> {
  val w2 = evv-current()
  if (evv-equal(w1,w2)) then cont(x) else unsafe-total{ error("non-scoped resumption") }
}

public fun ihandle( tag:htag<h>, h : h<e1,r>, action : () -> yld<e2,r> ) : yld<e1,r> {
  val m  = fresh-marker()
  val w1 = evv-current()
  val ev = Ev(tag,m,h,w1)
  val w2 = w1.evv-insert(ev)
  prompt(w1,m,under(w2,action))
}

extern inline unsafe-ref-to-local( r : ref<h,a> ) : local-var<h,a> { inline "#1" }
extern inline unsafe-st( action : (a) -> <div,st<h>|e> b ) : ((a) -> e b) { inline "#1" }

extern yld-local-var-create(x:a) : total (local-var<s,a>) {
  js inline "{value:#1}"
}

fun prompt-local-var(r:ref<h,a>, yld: yld< <local<h>|e>, b> ) : <div,st<h>> yld< <local<h>|e>, b> {
  match(yld) {
    Pure -> yld
    Yield(n,clause,cont) -> {
      val v = !r
      Yield(n,clause, unsafe-st( fun(x){ r := v; prompt-local-var(r,cont(x)) } ) )
    }
  }
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> yld< <local<s>|e>, b> ) : yld< <local<s>|e>, b> {
  unsafe-total {
    val r = ref(init)
    prompt-local-var(r,action(unsafe-ref-to-local(r)))
  }
}

fun lift( action : () -> e a ) : yld<e,a> {
  Pure(unsafe-coerce(action)())
}

// -------------------------------------------
// Effect handlers
// -------------------------------------------

type op<a,b,e,r> {
  Tail(op : (a -> yld<e,b>) )
  Control(op : (x:a, k: b -> yld<e,r>) -> yld<e,r> )
}

fun tail2(op : (a,b) -> yld<e,c> ) : op<(a,b),c,e,r> {
  Tail( fun(t:(_,_)){ op(t.fst,t.snd) } )
}

fun iyield( m:marker<e1,r>, clause:(k:((b) -> yld<e1,r>)) -> yld<e1,r> ) : yld<e,b> {
  val w = evv-current()
  Yield(m, clause, fun(x){ evv-set(w); Pure(x) })
}

public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> op<a,b,e1,r>), x : a ) : yld<e,b> {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail(clause) -> under(w,clause,x)
      Control(clause) -> iyield( m, fun(k){ clause(x,k) } )
    }
  }
}


type op2<a,b,c,e,r> {
  Tail2(op : (a,b) -> yld<e,c>)
  Control2(op : (x:a,y:b, k:(c) -> yld<e,r>) -> yld<e,r> )
}

public fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : yld<e,c> {
  match(evx) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail2(clause)    -> under(w,clause,x,y)
      Control2(clause) -> iyield(m, fun(k){ clause(x,y,k) })
    }
  }
}

// -------------------------------------------
// Example
// -------------------------------------------

type amb :: HX;

val amb-tag : htag<amb-hnd> = Unsafe-htag("amb")

type amb-hnd<e,r> {
  con Amb-hnd( op-flip : op<(),bool,e,r> , op-op2 : op<(int,double),bool,e,r> )
}

fun amb-handle( h : amb-hnd<e,r>, action : () -> yld<<amb|e>,r> ) : yld<e,r> {
  ihandle(amb-tag,h,action)
}

fun amb-flip( i : int ) : yld<<amb|e>,bool> {
  iperform(ievv-select(i,amb-tag),op-flip,())
}

public fun amb() {
  amb-handle(
    Amb-hnd( Control( fun(x,k){
                with xs : list<bool> = yld-bind( k(True) )
                with ys = yld-bind( k(False) )
                Pure(xs + ys)
             }),
             tail2( fun(x,y){ Pure(True) } )
    ))
  {
    with p = yld-bind( amb-flip(0) )
    Pure([p])
  }
}



type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : op<(),a,e,r>, op-set : op<a,(),e,r> )
}

fun state-handle( h : state-hnd<a,e,r>, action : () -> yld<<state<a>|e>,r> ) : yld<e,r> {
  ihandle(state-tag,h,action)
}

fun state-get( i : int ) : yld<<state<a>|e>,a> {
  iperform( ievv-select(i,state-tag), op-get, () )
}
fun state-set( i : int, x : a ) : yld<<state<a>|e>,()> {
  iperform( ievv-select(i,state-tag), op-set, x )
}

/*

val x = state-get(2)
yield-check(fun() { x + 2})
if (yielding()) then keep-yielding(fun(){ x + 2 })
x + 2

fun reflect() :
fun reify( action: () -> e a ) : yld<e,a> {

}
*/
/*
local-get : forall<s,a,e> (v: local-var<s,a>) -> <local<s>|e> a with(hdiv<s,a,e>)
~>
local-get : forall<s,a,e> (v: local-var<s,a>) -> e yld<a>  with(hdiv<s,a,e>)
State-hnd( Tail( fun(x){ Pure(local-get(s)) } ) )
*/
extern inline yld-local-scope : forall<e,a> ( action : forall<s> () -> yld< <local<s> | e>,a> ) -> yld<e,a>  {
  cs inline "Primitive.Run<##2>(#1)";
  js inline "((#1)())";
}

public fun add42() : yld<<>,int> {
  yld-local-scope {
    with s = ilocal-var(0)
    state-handle(
        State-hnd( Tail( fun(x){ lift{s} } ), Tail( fun(x){ lift{s := x} }) ),
        {
          with _x = yld-bind( state-set(0,42) )
          state-get(0 )
        }
    )
  }
}

/*
handle(action) {
  flip() -> resume(True)
  op1(x) ->
  op2(x,y) ->
}
~>
amb-handle( Amb-hnd(
   Control( fun((x,y),k){k(True)}) )
   Tail()
  ,action
)
*/

/*
fun map-acc(w,xs,f,acc) {
  while(1) {
  match(xs) {
    Nil {
      acc.reverse
    }
    Cons(x,xx) {
      map-acc(xx,f, Cons(f(x),acc))
      ~>
        val y = f(x)
        if (typeof y == Yield) ...
        map-acc(xx,f,Cons(y,acc))
      }
    }
  }
}
*/
