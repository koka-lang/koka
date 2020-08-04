effect reader {
  fun ask() : int
}

effect evil {
  control evil() : ()
}

val reader-one = handler { fun ask() { 1 } }

// forall<a,e>. (() -> <evil/evil|e> (() -> e a)) -> e (() -> e a)
//forall<a,e>. (() -> <evil/evil|e> (() -> e a)) -> e (() -> e a)
val hevil = handler { 
  control evil() { (fun(){ resume(())() } ) } 
}

fun test() : <evil,reader> (() -> int) {
  val x = ask()
  evil() // Commenting out this line produces an error
  (fun(){ x })
}

fun non-scoped() : int {
  val f : () -> reader int
        = reader-one{ 
            hevil{ test() } 
        }
  reader-one{ f() }
}


fun main(){
    println(non-scoped())
}
