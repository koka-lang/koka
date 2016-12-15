// test if we keep effects polymorph
// ok     : forall<a,e> (f: (x: a) -> e a) -> ((x: a) -> e a)
function ok(f) -> choose(id,f)
