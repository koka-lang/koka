public module ev3

/*
type exc :: HX;

val exc-tag : htag<exc-hnd> = hidden-htag("exc")

type exc-hnd<e,r> {
  con Exc-hnd( op-throwx : forall<a> clause1<string,a,e,r> )
}

fun handle-exc( h : exc-hnd<e,r>, ret: a -> e r, action : () -> e a) : e r {
  hidden-handle(exc-tag,h,ret,action)
}

val cls-fun = (fun(s,k){ Nothing })
val to-maybe-cls = clause-control1(cls-fun)
val to-maybe-hnd = Exc-hnd(to-maybe-cls)


fun to-maybe(action) {
  handle-exc( // Exc-hnd(to-maybe-cls)
              Exc-hnd(clause-control1(cls-fun))
              , Just, action )
}
*/


effect exc {
  control bad() : a
}

fun to-maybe(action) {
  handle(action) {
    return x      { Just(x) }
    control bad() { Nothing }
  }
}

fun test() {
  to-maybe{
    42 + bad()
  }
}
