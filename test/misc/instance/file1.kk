// Test instance handler

ambient filesys<s::S> { }

fun filesys( action : forall<s> () -> <filesys<s>|e> a ) : e a {
  with<filesys<_>> { }
  action()
}

ambient instance file<s::S> in filesys<s> {
  fun read() : string
}

fun fake-file(content,action) {
  with f = instance fun read() { content }  // ~> (with new { fun read() {..} })(fn(f) { ... })
  action(f)
}

fun main() {
  with<filesys> { }                // ~> with with<filesys> { }
  with f = fake-file( "hi" )       // ~> fake-file("hi", fn(f) { ... } }
  with g = fake-file("there")
  println(f.read() + g.read())
}
