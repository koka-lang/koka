module ev1
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
  con Ev<e,r>(:htag<h>, :marker<e,r>, :h<e,r>)
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

// -------------------------------------------
// Yield monad
// -------------------------------------------

abstract rectype yld<e::E,a> {
  Pure(x:a)
  Yield<b,e1,r>(marker:marker<e1,r>, clause:(k:((evv<e1>,b) -> yld<e1,r>)) -> yld<e1,r>, cont:(b -> yld<e,a>))
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

public fun yld-run(yld : yld<<>,a>) : exn a {
  match(yld) {
    Pure(x) -> x
  }
}
extern inline unsafe-coerce(x:a) : b { inline "#1" }

fun prompt( w:evv<e1>, m : marker<e1,r>, res : yld<e,r> ) : yld<e1,r> {
  match(res) {
    Pure(x) -> Pure(x)
    Yield(n,clause,cont) {
      val deep = fun(x){ prompt(w,unsafe-decreasing(m),cont(x)) }
      if (m == n) then unsafe-coerce(clause)(fun(w2,x){ guard(w,deep,w2,x) }) else Yield(n,clause,deep)
    }
  }
}

fun guard( w1 : evv<e>, cont : a -> yld<e,b>, w2 : evv<e>, x:a ) : yld<e,b> {
  if (evv-equal(w1,w2)) then cont(x) else unsafe-total{ error("non-scoped resumption") }
}

public fun ihandle( w1 : evv<e1>, tag:htag<h>, h : h<e1,r>, action : (w : evv<e2>) -> yld<e2,r> ) : yld<e1,r> {
  val m  = fresh-marker()
  val ev = Ev(tag,m,h)
  val w2 = w1.evv-insert(ev)
  prompt(w1,m,action(w2))
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

// -------------------------------------------
// Effect handlers
// -------------------------------------------

type op<a,b,e,r> {
  Tail(op : (a -> yld<e,b>) )
  Control(op : (x:a, k:(evv<e>,b) -> yld<e,r>) -> yld<e,r> )
}

fun tail2(op : (a,b) -> yld<e,c> ) : op<(a,b),c,e,r> {
  Tail( fun(t:(_,_)){ op(t.fst,t.snd) } )
}

public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> op<a,b,e1,r>), x : a ) : yld<e,b> {
  match(ev) {
    Ev(_tag,m,h) -> match(h.op) {
      Tail(clause)    -> unsafe-coerce(clause(x))
      Control(clause) -> Yield(m, fun(k){ clause(x,k) }, Pure)
    }
  }
}


type op2<a,b,c,e,r> {
  Tail2(op : (a,b) -> yld<e,c>)
  Control2(op : (x:a,y:b, k:(evv<e>,c) -> yld<e,r>) -> yld<e,r> )
}

public fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : yld<e,c> {
  match(evx) {
    Ev(_tag,m,h) -> match(h.op) {
      Tail2(clause)    -> unsafe-coerce(clause(x,y))
      Control2(clause) -> Yield(m, fun(k){ clause(x,y,k) }, Pure)
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

fun amb-handle( w : evv<e>, h : amb-hnd<e,r>, action : (w:evv<<amb|e>>) -> yld<<amb|e>,r> ) : yld<e,r> {
  ihandle(w,amb-tag,h,action)
}

public fun amb(w) {
  amb-handle(
    w,
    Amb-hnd( Control( fun(x,k){
                with xs : list<bool> = yld-bind( k(w,True) )
                with ys = yld-bind( k(w,False) )
                Pure(xs + ys)
             }),
             tail2( fun(x,y){ Pure(True) } )
    ),
    fun(w2) {
      with p = yld-bind( iperform(w2.ievv-select-at(0),op-flip,()) )
      Pure([p])
    }
  )
}



type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : op<(),a,e,r>, op-set : op<a,(),e,r> )
}

fun state-handle( w : evv<e>, h : state-hnd<a,e,r>, action : (w:evv<<state<a>|e>>) -> yld<<state<a>|e>,r> ) : yld<e,r> {
  ihandle(w,state-tag,h,action)
}
/*
public fun amb(w) {
  amb-handle(
    w,
    Amb-hnd( Control( fun(x,k){
                with xs : list<bool> = yld-bind( k(w,True) )
                with ys = yld-bind( k(w,False) )
                Pure(xs + ys)
             }),
             tail2( fun(x,y){ Pure(True) } )
    ),
    fun(w2) {
      with p = yld-bind( iperform(w2.ievv-select-at(0),op-flip,()) )
      Pure([p])
    }
  )
}
*/
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
