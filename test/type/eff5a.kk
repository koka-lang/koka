// test if we keep effects polymorph
// ok     : forall<e,a> (f: (string) -> e a) -> ((string) -> e a) with (partial<=e)
function ok(f) -> choose(error,f)
