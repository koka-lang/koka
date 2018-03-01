// Test resource handler
//
// This is a variant where we do not need a parent `join` handler
// but we handle directly inside the resource handlers.
// This only works if we have an `inject-resource` operation

effect corrl { }

fun corrl( action : () -> <corrl|e> a ) : e a {
  handle<corrl>(action) { }
}


fun interleaved(action1,action2) {
  // dummy implementation
  action1()
  action2()
}

effect resource stream<a> in corrl {
  fun push( value : a ) : ()   // introduces `push : (stream<l,a>, a) -> <l|e> ()`
}

// if we have generative types...
//new-stream : forall<a,b,e> fresh<l> ((stream<l,a> -> <l|e> b)) -> e b
val new-stream = handler resource (s) {
  // dummy implementation
  push(x) -> resume((),x)
}

fun align( s1 : stream<a>, s2 : stream<b>, action : () -> <corrl,pure|e> () ) : <corrl,pure|e> ()
{
  // we use this wrapper to help Koka figure out the `st<h>` effect can be discharged
  alignx(s1,s2,inject-st(action))
}

fun alignx(// generative types: s1 : stream<l1,a>, s2 : stream<l2,b>, action : () -> <l1,l2|e> c) : <l1,l2|e> c
          s1 : stream<a>, s2 : stream<b>, action : () -> <corrl,st<h>,pure|e> () ) : <corrl,st<h>,pure|e> ()
{
  var st1 := Nothing
  var st2 := Nothing

  // Here the `handler resource s1` has an extra parameter `s1` and will
  // not create a fresh resource, but instead handle operations for `s1`.
  val h1 = handler resource s1 {
    push(x) -> match(st2) {
      Nothing -> st1 := Just((x,resume))  // and do not resume
      Just((y,resumey)) -> {
        s2.push(y)
        s1.push(x)
        interleaved { resume(()) } { resumey(()) }  //??
      }
    }
  }
  val h2 = handler resource s2 {
    push(y) -> match(st1) {
      Nothing -> st2 := Just((y,resume))
      Just((x,resumex)) -> {
        s2.push(y)
        // subtle: skip the outer h1 handler! (`inject-resource` is auto generated)
        s1.inject-resource{ s1.push(x) }
        interleaved { resumex(()) } { resume(()) }  //??
      }
    }
  }

  h1{ h2( action ) }
}

fun main() {
  using corrl
  use s1 = new-stream(0)
  use s2 : stream<bool> = new-stream(True)  // stream<_l,bool> with generative types
  using align(s1,s2)
  interleaved {
    s1.push(1)
    s1.push(2)
  }
  { s2.push(True)
    s2.push(False)
  }
  println("hi")
}
