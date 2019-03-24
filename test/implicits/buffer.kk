// with expression syntax
implicit val get-buffer: string
implicit fun set-buffer(s: string): ()


// Monomorphic since we need a type annotation on the continuation!
fun withBuffer(init: string, action: () -> <get-buffer,set-buffer> string): string {
  with val get-buffer = init
  with control set-buffer(s : string) {
    val k: (()) -> <get-buffer> string = resume
    with val get-buffer = s
    k(())
  }
  action()
}

implicit fun emit(s: string): ()

fun emit-collect(action) {
  with fun emit(s) { set-buffer(get-buffer + s + "\n") }
  action()
}

// withBuffer("", { emit-collect({ emit("Hello"); emit("World") }); get-buffer })
