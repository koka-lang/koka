
extern include {
  js file "yld.js"
}

// --------------------------
abstract struct marker<e::E,a>(m:int)

type evv<e::E>;

type ev<h> {
  con Ev<e,r>(:string, :marker<e,r>, :hnd<h,e,r>)
}

struct hnd<h,e::E,r>(hnd:h<e,r>)

struct tag<h::(E,V)->V>(tag:string)

extern evv-total() : evv<<>> {
  js inline "[]"
}

extern evv-insert( ev : ev<h>, evv : evv<e1> ) : evv<e2> {
  js "evv_insert_ev"
}

extern evv-select( tag : tag<h>, i : int, evv : evv<e> ) : ev<h> {
  js inline "(i >= 0 ? evv[i] : evv_lookup(tag,evv))"
}

rectype yld<e::E,a> {
  Pure(x:a)
  Yield<b,e1,r>(marker:marker<e1,r>, clause:((k:(b -> yld<e1,r>)) -> yld<e1,r>), cont:(b -> yld<e,a>))
}

type op<a,b,e,r> {
  Tail(op : (a -> yld<e,b>) )
  Control(op : (x:a, k:(b -> yld<e,r>)) -> yld<e,r> )
}

// -----------------------------
type amb :: HX;

val amb-tag : tag<amb-hnd> = Tag("amb")

type amb-hnd<e,r> {
  con Amb-hnd( op-flip : op<(),bool,e,r> )
}
