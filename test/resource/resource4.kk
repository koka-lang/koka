// Test resource handler


// Some overall effect in which all the resource operations take place
effect corrl { }

fun corrl( action : () -> <corrl|e> a ) : e a {
  handle<corrl>(action) { }
}

// The streams are resources, i.e. identifiable handlers (as generative effects)
// Generative effects could do this too and with proper typing where an effect
// operation is always within a handler scope. In our case, we put all resource
// operations in one common effect and thus operations might escape their
// particular handler scope. However, if we reflect every resource in its own
// generative type, we might get effects types that become too cumbersome...
effect resource stream<a> in corrl {
  fun push( value : a ) : ()   // introduces `push : (stream<l,a>, a) -> <l|e> ()`
}

// The type of new-stream is:
//   new-stream: forall<a,b,e> ((stream<a> -> <corrl|e> b) -> <corrl|e> b)
// i.e. it creates a fresh stream resource that is handed by this particular
// handler. No effect is discharged; all streams are under `corrl`.
//
// if we would have generative effect, the type could be:
//  new-stream : forall<a,b,e> fresh<l> ((stream<l,a> -> <l|e> b)) -> e b
val new-stream = handler resource (s) {
  // dummy implementation
  push(x) -> resume((),x)
}

fun interleaved(action1,action2) {
  // dummy implementation
  action1()
  action2()
}



// Private effect and data type to handle stream alignment
private effect join<a,b> {
  fun left( x : a ) : ()
  fun right( x : b ) : ()
}

private type joined<a,b,e> {
  JoinNone
  JoinLeft(:a, : resume-context<(),e,()>)
  JoinRight(:b, : resume-context<(),e,()>)
}


fun align( s1: stream<a>, s2 : stream<b>, action : () -> <corrl,pure|e> () ) : <corrl,pure|e> () {
  // we need to wrap this to help Koka figure out that `st<h>` can be discharged
  // - the `div` effect comes in because we store higher order functions in the heap
  // - the `exn` effect comes in because resource operations might not be handled
  alignx(s1,s2,inject-st(action))
}

fun alignx(s1,s2,action)
{
  // the streams are joined by the join handler
  var st := JoinNone

  fun joined( x, y, contextx, contexty ) {
    s1.push(x)
    s2.push(y)
    interleaved { contextx.resume(()) } { contexty.resume(()) }
  }

  val hjoin = handler {
    fun raw left(x) -> match(st) {
      JoinRight(y,resumey) -> joined(x,y,context,resumey)
      _ -> st := JoinLeft(x,context)
    }
    fun raw right(y) -> match(st) {
      JoinLeft(x,resumex) -> joined(x,y,resumex,context)
      _ -> st := JoinRight(y,context)
    }
  }

  // The resource handlers delegate to the join handler
  // Here the `handler resource s1` has an extra parameter `s1` and will
  // not create a fresh resource, but instead handle operations for `s1`.
  val h1 = handler resource s1 {
    push(x) -> left(x)
  }
  val h2 = handler resource s2 {
    push(y) -> right(y)
  }

  // we inject `join` so we are properly composed, anyone inside `action`
  // should not be able to access our join handler!
  hjoin{ h1{ h2{ inject<join>(action) } } }
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
