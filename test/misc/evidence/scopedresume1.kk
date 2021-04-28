effect reader {
  fun ask() : int
}

effect evil {
  control evil() : ()
}

val reader-one = handler { fun ask(){ 1 } }
val reader-two = handler { fun ask(){ 2 } }

val hevil = handler { 
  return x { (fn(){ x }) }
  control evil() { (fn(){ resume(())() } ) } 
}

fun test() : <reader,evil> int {
  val x = ask()
  evil()
  val y = ask()
  x + y
}

// scoped resumptions: fails at runtime
// regular effects: returns 3  (not 2)
fun non-scoped() : int {
  val f // : () -> reader int
        = reader-one{ hevil{ test() } }
  reader-two{ f() }
}

// -----------------------------------

// outer handler that captures a full resumption
effect outer {
  control ret() : ()
}

val outer = handler {
  control ret(){ (fn(){ resume(())() }) }
}

// special reader that can "change" 
effect xreader {
  fun xask() : int
  fun xinc() : ()
}

fun xreader(action) {
  var x := 1
  handle( {mask<local>(action)} ) {
    fun xask() { x }
    fun xinc() { x := x + 1 }
  }
}

// evil coordinates the change
val xevil = handler{ 
  return x { (fn(){ x }) }
  control evil() { 
    ret()       // capture resumption to outer (which returns a full resumption)
    xinc()      // if resumed (through outer), change the reader    
    resume(())  // and resume from here
  }
}


fun xtest() : <xreader,evil> int {
  val x = xask()
  evil()
  val y = xask()
  x + y
}

// now we can stay scoped
// note: we can add a parameter (returned from ret()) to call `f`
// multiple times with a changed reader depending on the parameter.
fun scoped() : int {
  val f : () -> int
        = outer{ xreader{ xevil{ xtest() } } }
  f()
}
