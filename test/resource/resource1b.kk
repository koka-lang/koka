// --------------------------------------------------------
// resources
// test with safe resources using rank-2 types to prevent
// escaping the correct scope
// --------------------------------------------------------

// Test resource handler

// We use `:filesys` to scope file operations. Here the effect
// is empty but in general there might be operations; for example
// to implement lower level file operations and delimit the `io` effect.
effect filesys<s::S> { }

// wrap in a rank-2 type to prevent `:file` resources to escape the
// scope of the handler. This is not strictly necessary as a runtime exception
// is generated anyway if a resource is used outside its handler.
// todo: add a `scoped` keyword to an effect declaration to do this automatically.
fun filesys( action : forall<s> () -> <filesys<s>|e> a ) : e a {
	handle<filesys>(action){  }
}

// resources can now be created inside a `:filesys` effect.
// note: the type of read is `:(file<s>) -> <filesys<s>,exn> string`.
// The `:exn` is added since we cannot statically guarantee (without adding more
// phantom rank-2 types) that a `:file` resource is accessed within the scope
// of its handler. If it is accessed outside the scope an exception is raised.
effect resource file<s::S> in filesys<s> {
	fun read() : string
}


// Each individual resource can have a custom handler
fun fake-file(content,action) {  // fake-file: (string, (file<s>) -> <filesys<s>|e> a ) -> <filesys<s>|e> a
	handle resource (action) {
		return x 	-> x
		read()	-> resume(content)
	}
}


effect heap<s::S> { }

fun heap( action : forall<s> () -> <heap<s>|e> a ) : e a {
	handle<heap>(action) { }
}

effect resource ref<s::S,a::V> in heap<s> {
	fun get() : a
	fun set( value : a ) : ()
}

// Create new polymorphic references.
// note: a drawback for "references as resources" is that they are scoped
// under a handler now: you cannot access the reference outside the `new-ref` scope.
val new-ref = handler resource(s) {   // new-ref: (init:a, action: (ref<s,a>) -> <heap<s>|e> b) -> <heap<s>|e> b
	get() 	-> resume(s,s)
	set(x) 	-> resume((),x)
}

fun main() {
	using heap
	using filesys
	use r1 = new-ref(36)
	use r2 : ref<_,string> = new-ref("hi")
	use f = fake-file("test")
	println( r1.get + r2.get.count + f.read.count)
}

/* note: `use` and `using` are syntactic sugar
   for scoped operations:


   use x = f(e1,..,en)
   ...
~>
   f(e1,..,en,fun(x){ ... })


   using f(e1,..,en)
   ...
~>
   f(e1,..,en,fun(){ ... })

*/
