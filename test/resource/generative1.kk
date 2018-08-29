// --------------------------------------------------------
// Emulate generative types
// --------------------------------------------------------

effect heap<s::S,a::V>{
  fun get( r : ref<s,a> ) : a
  fun set( r : ref<s,a>, value : a ) : ()
}

abstract type ref<s::S,a::V> {
  Ref
}

fun new-ref( initial : a, action : forall<s> (ref<s,a>) -> <heap<s,a>|e> b ) : e b {
  handle({action(Ref)}) (st = initial) {
    get(_r) -> resume(st,st)
    set(_r,x) -> resume((),x)
  }
}

fun bar( r1 : ref<s1,a>, r2 : ref<s2,b> ) : <heap<s1,a>,heap<s2,b>|e> (a,b) {
  (r1.get(), inject<heap>{r2.get()})
}

fun foo()  {
  use r1 = new-ref(50)
  use r2 = new-ref(False)
  println( r2.get() : bool  )
  println( inject<heap>{r1.get()} : int)
  val (x,_) = bar(r2,r1)
  println(x)
  /*
  // <heap<$s1,int>|_e> ~ <heap<$s2,bool>, heap<$s1,int>,..>

  // <heap<$s2,bool>, heap<$s1,int>,..>
  // <heap<$s1,int>, heap<$s2,bool>,..>

  // but this fails due to effect order :-(
  val (a,b) = bar(r1,r2)
  println(a)
  */
}
