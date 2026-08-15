module specbox-lib

// Helper for specbox.kk: a polymorphic function with an implicit `?(==)` and a
// recursive helper, so a `specialize "___*"` mask is derived and its body is
// emitted into the .kki inline section.
//
// Guards against overlapping type-variable ids handed out by the interface
// reader: with those, the specialized definition keeps a raw id in its result
// type and the generated C fails to compile with kk_box_t errors.
//
// Precompiled in test/Spec.hs (like mpat-lib.kk) so specbox.kk loads it from the
// .kki -- compiling both in one invocation never reads an interface. Needs -O2
// (see the .flags files): the specialize pass is what consumes the inline def.
pub type vmap<a,b>
  Vmap( items : list<(a,b)> )

fun go( xs : list<(a,b)>, k : a, v : b, ?(==) : (a,a) -> e bool ) : e list<(a,b)>
  match xs
    Nil -> [(k,v)]
    Cons((k2,v2), rest) -> if k2 == k then Cons((k,v), rest)
                           else Cons((k2,v2), go(rest, k, v))

pub fun set( m : vmap<a,b>, k : a, v : b, ?(==) : (a,a) -> e bool ) : e vmap<a,b>
  match m
    Vmap(l) -> Vmap(go(l, k, v))

pub fun elems( m : vmap<a,b> ) : list<(a,b)>
  match m
    Vmap(l) -> l

pub fun main()
  println( Vmap([]).set(1,10).elems.length )
