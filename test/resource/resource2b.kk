// Test resource handler

effect filesys<s::S> { }

fun filesys( action : forall<s> () -> <filesys<s>|e> a ) : e a {
  handle<filesys<_>>(action){ }
} 

effect resource file<s::S> in filesys<s> {
  fun read() : string
}


fun fake-file(content,action) {
  handle resource (action) {
    return x -> x
    read() -> resume(content)
  }
}

fun main() {
  using filesys
  use f = fake-file( "hi" ) 
  println(f.read.count + 40)
}