// Test resource handler

effect corrl { }

fun corrl( action : () -> <corrl|e> a ) : e a {
  handle<corrl>(action) { }
}

effect resource stream<a> {
  fun push( value : a ) : ()   // introduces `push : (stream<l,a>, a) -> <l|e> ()`
}

// if we have generative types...
//new-stream : forall<a,b,e> fresh<l> ((stream<l,a> -> <l|e> b)) -> e b
val new-stream = handler resource(s) {
  push(x) -> resume((),x)
}

fun align(// generative types: s1 : stream<l1,a>, s2 : stream<l2,b>, action : () -> <l1,l2|e> c) : <corrl,exn|e> c 
          s1 : stream<a>, s2 : stream<b>, action : () -> <corrl,exn|e> c ) : <corrl,exn|e> c
{
  var st := Nothing
  fun update(sx,x,resumex) {
    match(st) {
      Nothing -> st := Just((sx,x,resumex))
      Just((sy,y,resumey)) -> {
        sy.push(y)
        sx.push(x)
        interleave(resumey,resumex)
      }
    }
  }
  
  // todo: does not work yet; but the idea is to write a handler for a specific resource, like `s1`
  val h1 = handler resource @ s1 {
    push(x) -> update(s1,x,resume)
  }
  val h2 = handler resource @ s2 {
    push(x) -> update(s2,x,resume)
  }

  h1(h2(action))
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