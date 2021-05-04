effect eff1 { 
  control op1() : ()
}

rec effect  eff2 {
  control op2() : (() -> <eff1,eff2> ())
}

fun h1( action : () -> <eff1|e> () ) : e () {
  with fun op1() { () }
  action()
}

fun f1() : <eff1> () {

}

fun f2( k : (() -> <eff1> ()) -> _ () ) : _ () {
  k( fn(){ k(fn(){ op1() }) } )
}

fun h2( action : () -> <eff1,eff2> () ) : <eff1> () {
  with control op2() { 
    // resume( fn(){ resume(fn(){ op1() }) } ) 
    f2(resume)
  }
  action()
}

fun main() {
  h1{ h2{ h1{ op2()() }} }
}