fun choose(x,y){ if (True) then x else y}
fun error(msg:string){ throw(msg) }

// test if we keep effects polymorph
// ok     : forall<e,a> (f: (string) -> e a) -> ((string) -> e a) with (partial<=e)
fun ok(f) { choose(error,f) }
