// Test resource handler

effect filesys<s::S> { }

fun filesys( action : forall<s> () -> <filesys<s>|e> a ) : e a {
  handle<filesys<_>>(action){ }
}

effect named file<s::S> in filesys<s> {
  fun read() : string
}

fun fake-file(content,action) {
  with f = named fun read() { content }  // ~> (with named { fun read() {..} })(fun(f) { ... })
  action(f)
}

fun main() {
  with<filesys> { }                // ~> with with<filesys> { }
  with f = fake-file( "hi" )       // ~> fake-file("hi", fun(f) { ... } }
  println(f.read.count + 40)
}
