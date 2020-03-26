module ev2
import std/num/int32

extern include {
  js file "yld.js"
}


// -------------------------------------------
// Internal types
// -------------------------------------------

struct marker<e::E,a>(m:int32)

//struct hnd<h,e::E,r>(hnd:h<e,r>)

public type htag<h::(E,V)->V> {
  Unsafe-htag(tag:string)
}

type ev<h> {
  con Ev<e,r>(:htag<h>, :marker<e,r>, :h<e,r>, :evv<e>)
}

type evv<e::E>;


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

public extern ievv-lookup( evv : evv<e>, htag : htag<h> ) : ev<h> {
  js "evv_lookup"
}

public extern inline ievv-select-at( evv : evv<e>, i : int ) : ev<h> {
  js inline "(#1)[#2]"
}

public fun ievv-select( evv : evv<e>, i : int, htag : htag<h> ) : ev<h> {
  if (i>=0) then evv.ievv-select-at(i) else evv.ievv-lookup(htag);
}


extern fresh-marker-int() : int32 {
  js inline "marker_count++"
}

fun fresh-marker() : marker<e,r> {
  Marker(fresh-marker-int())
}

fun (==)(m1 : marker<e1,a1>, m2: marker<e2,a2>) : bool {
  (m1.m == m2.m)
}

extern evv-current() : evv<e> {
  js inline "$evv"
}

extern evv-set(w : evv<e>) : () {
  js inline "$evv = #1"
}

fun under( w : evv<e1>, action : () -> e1 yld<a> ) : e yld<a> {
  val w0 = evv-current();
  evv-set(w)
  val yld = unsafe-coerce(action)
  evv-set(w0)
  yld
}

fun under( w : evv<e1>, action : (a) -> e1 yld<b>, x : a ) : e yld<b> {
  val w0 = evv-current();
  evv-set(w)
  val yld = unsafe-coerce(action)(x)
  evv-set(w0)
  yld
}

fun under( w : evv<e1>, action : (a,b) -> e1 yld<c>, x : a, y : b ) : e yld<c> {
  val w0 = evv-current();
  evv-set(w)
  val yld = unsafe-coerce(action)(x,y)
  evv-set(w0)
  yld
}

// -------------------------------------------
// Yield monad
// -------------------------------------------

abstract rectype yld<a> {
  Pure(x:a)
  Yield<b,e,r>(marker:marker<e,r>, clause: (k: (b) -> e yld<r>) -> e yld<r>, cont:(b -> yld<a>))
}


public fun yld-pure(x : a) : yld<a> {
  Pure(x)
}

fun kcompose(f : b -> e yld<c>, g : a -> e yld<b>, x : a ) : e yld<c> {
  match(g(x)) {
    Pure(y) -> f(y)
    Yield(m,clause,cont) -> Yield(m,clause,fun(y){ kcompose(unsafe-decreasing(f),cont,y) })
  }
}

public fun yld-bind(yld : yld<a>, next : (a -> e yld<b>)) : e yld<b> {
  match(yld) {
    Pure(x) -> next(x)
    Yield(m,clause,cont) -> Yield(m,clause,fun(x){ kcompose(next,cont,x) })
  }
}

public fun yld-run(yld : () -> <> yld<a>) : exn a {
  under(evv-total()) {
    match(yld()) {
      Pure(x) -> x
    }
  }
}
extern inline unsafe-coerce(x:a) : b { inline "#1" }

fun prompt( w : evv<e>, m : marker<e,r>, res : yld<r> ) : e yld<r> {
  match(res) {
    Pure(x) -> Pure(x)
    Yield(n,clause,cont) {
      val deep = fun(x){ prompt(w,unsafe-decreasing(m),cont(x)) }
      if (m == n) then unsafe-coerce(clause)(fun(x){ guard(w,deep,x) }) else Yield(n,clause,deep)
    }
  }
}

fun guard( w1 : evv<e1>, cont : a -> e yld<b>, x:a ) : e yld<b> {
  val w2 = evv-current()
  if (evv-equal(w1,w2)) then cont(x) else unsafe-total{ error("non-scoped resumption") }
}

public fun ihandle( tag:htag<h>, h : h<e1,r>, action : () -> e2 yld<r> ) : e1 yld<r> {
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

fun prompt-local-var(r:ref<h,a>, yld: yld<b> ) : <div,st<h>> yld<b> {
  match(yld) {
    Pure -> yld
    Yield(n,clause,cont) -> {
      val v = !r
      Yield(n,clause, unsafe-st( fun(x){ r := v; prompt-local-var(r,cont(x)) } ) )
    }
  }
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> yld<b> ) : <local<s>|e> yld<b> {
  unsafe-total {
    val r = ref(init)
    prompt-local-var(r,action(unsafe-ref-to-local(r)))
  }
}

// -------------------------------------------
// Effect handlers
// -------------------------------------------

type op<a,b,e,r> {
  Tail(op : (a -> e yld<b>) )
  Control(op : (x:a, k: b -> e yld<r>) -> e yld<r> )
}

fun tail2(op : (a,b) -> e yld<c> ) : op<(a,b),c,e,r> {
  Tail( fun(t:(_,_)){ op(t.fst,t.snd) } )
}

fun iyield( m:marker<e1,r>, clause:(k:((b) -> e1 yld<r>)) -> e1 yld<r> ) : e yld<b> {
  val w = evv-current()
  Yield(m, clause, fun(x){ evv-set(w); Pure(x) })
}

public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> op<a,b,e1,r>), x : a ) : e yld<b> {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail(clause) -> under(w,clause,x)
      Control(clause) -> iyield( m, fun(k){ clause(x,k) } )
    }
  }
}


type op2<a,b,c,e,r> {
  Tail2(op : (a,b) -> e yld<c>)
  Control2(op : (x:a,y:b, k:(c) -> e yld<r>) -> e yld<r> )
}

public fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : e yld<c> {
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

fun amb-handle( h : amb-hnd<e,r>, action : () -> <amb|e> yld<r> ) : e yld<r> {
  ihandle(amb-tag,h,action)
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
    with p = yld-bind( iperform(evv-current().ievv-select-at(0),op-flip,()) )
    Pure([p])
  }
}



type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : op<(),a,e,r>, op-set : op<a,(),e,r> )
}

fun state-handle( h : state-hnd<a,e,r>, action : () -> <state<a>|e> yld<r> ) : e yld<r> {
  ihandle(state-tag,h,action)
}

public fun add42() {
  with s = ilocal-var(0)
  state-handle(
      State-hnd( Tail( fun(x){ Pure(s) } ), Tail( fun(x){ s := x; Pure(()) }) ),
      {
        iperform( evv-current().ievv-select-at(0), op-get, () )
      }
  )
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
