pub module ev-examples1

import std/core/hnd

type cow :: HX  {
  private con Cow<e,r>( : cow-hnd<e,r> )
}

rectype cow-hnd<e,r> {
  con Cow-hnd( op-moo: clause0< () -> cow (), e, r> )
}

// -------------------------------------------
// Example
// -------------------------------------------

type exc :: HX;

val exc-tag : htag<exc-hnd> = hidden-htag("exc")

type exc-hnd<e,r> {
  con Exc-hnd( op-throwx : forall<a> clause1<string,a,e,r> )
}

fun ethrow( s : string ) : exc a {
  hidden-perform1( hidden-evv-lookup(exc-tag), op-throwx, s )
}

type amb :: HX;

val amb-tag : htag<amb-hnd> = hidden-htag("amb")

type amb-hnd<e,r> {
  con Amb-hnd( op-flip : clause0<bool,e,r> )
}

fun amb-handler( h : amb-hnd<e,r>, ret : a -> e r ) : ((action:() -> <amb|e> a) -> e r) {
  hidden-handler(amb-tag,h,ret)
}

fun amb-handle( h : amb-hnd<e,r>, ret : a -> e r, action : () -> <amb|e> a ) : e r {
  hidden-handle(amb-tag,h,ret,action)
}

fun amb-flip() : <amb|e> bool {
  hidden-perform0(hidden-evv-lookup(amb-tag),op-flip)
}

val h = amb-handler( Amb-hnd( clause-control0( fn(k) {
            with xs : list<_> = yield-bind(k(False))
            with ys = yield-bind( k(True) )
            xs + ys
         })), fn(x){ [x] } )

fun amb(action) {
  h(action)
}

fun amb2(action) {
  amb-handle( Amb-hnd( clause-control0( fn(k) {
                with xs : list<_> = yield-bind(k(False))
                with ys = yield-bind( k(True) )
                xs + ys
              })),
              fn(x){ [x] },
              action )
}

fun xor() : amb bool {
  with p:bool = yield-bind(amb-flip())
  with q = yield-bind(amb-flip())
  (p!=q)
}

fun test-amb() : list<bool> {
  amb(xor)
}

// Reader with control
type reader<a> :: V -> HX;

val reader-tag : forall<a> htag<reader-hnd<a>> = hidden-htag("reader")

struct reader-hnd<a,e,r>( op-read : clause0<a,e,r> )

fun read-handle( h : reader-hnd<a,e,r>, ret : b -> e r, action : () -> <reader<a>|e> b) : e r {
  hidden-handle(reader-tag,h,ret,action)
}

fun read() {
  hidden-perform0(hidden-evv-lookup(reader-tag), op-read )
}

fun reader(init,action) {
  read-handle( Reader-hnd( clause-control0(fn(k){ k(init) } )), id, action)
}

// State
type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = hidden-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : clause0<a,e,r>, op-set : clause1<a,(),e,r> )
}

fun state-handle( h : state-hnd<a,e,r>, ret : b -> e r, action : () -> <state<a>|e> b ) : e r {
  hidden-handle(state-tag,h,ret,action)
}

fun state-get() : <state<a>|e> a {
  hidden-perform0( hidden-evv-lookup(state-tag), op-get )
}
fun state-set( x : a ) : <state<a>|e> () {
  hidden-perform1( hidden-evv-lookup(state-tag), op-set, x )
}


fun state(init : s, action : () -> <state<s>|e> a) : e a {
  with s = hnd/local-var(init)
  state-handle(State-hnd(
    clause-tail-noyield0( fn(){ s } ),
    clause-tail-noyield1( fn(x){ s := x }) ),
    id, { mask<local>(action) } )
}

fun add21() : state<int> int {
  with i  = yield-bind(state-get())
  with _x = yield-bind(state-set(i+21))
  state-get()
}

fun test-state() : int {
  state(21) {
    add21()
  }
}

fun inc() : state<int> int {
  with i = yield-bind(state-get())
  with _x = yield-bind(state-set(i+1))
  i
}

fun xorst() {
  with p = yield-bind(xor())
  with i = yield-bind(inc())
  with j:int = yield-bind(read())
  i+j
}

fun test-state-amb() : list<int> {  // 1,2,3,4
  with reader(1)
  with state(0)
  with amb
  xorst()
}

fun test-amb-state() : list<int> {  // 1,1,1,1
  with reader(1)
  with amb
  with state(0)
  xorst()
}


// Tests
fun state-x(init,action) {
  with s = hnd/local-var(init)
  state-handle(State-hnd(
    clause-tail0( fn(){ read() } ),
    clause-tail-noyield1( fn(x){ s := x }) ),
    id, action)
}

fun test-difficult() : int {
  with reader(1)
  with state-x(42)
  //state-get(-1)
  with reader(2)
  with x:int = yield-bind(state-get())
  with y = yield-bind(read())
  (x+y)
}

fun test() {
  test-amb().show-list(show).println
  test-state().println
  test-difficult().println
  test-state-amb().show-list(show).println
  test-amb-state().show-list(show).println
}
