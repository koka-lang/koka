type sym0
type sym1

// a tape
type cons<a,b>    // tape with head `a` and tail `b`
type inf          // all states also match on `inf` and produce `cons<sym0,inf>` to simulate infinite tape
type state-a 
type state-b
type state-c

// the machine configuration: <state, left tape, right tape>
abstract type config<s,a,c>
  Start

// starting machine configuration with all zeros
val start-config : config<state-a,inf,cons<sym0,inf>> = Start 

// halting state
fun halt/trans() : ()  
  ()

// stateA: if head==0 then write 1, shift the tape right, and continue in state B
fun a/b/trans( st : config<state-a,cons<x,l>,cons<sym0,r>>, 
               ?trans : (config<state-b,l,cons<x,cons<sym1,r>>>) -> () ) : ()
  ()
// stateA: if head==1 then write 1, shift the tape left, and continue in state C
fun a/c/trans( st : config<state-a,l,cons<sym1,cons<x,r>>>, 
               ?trans : (config<state-c,cons<sym1,l>,cons<x,r>>) -> () ) : ()
  ()
// stateB: if head==0 then write 1, shift the tape left, and continue in state A
fun b/a/trans( st : config<state-b,l,cons<sym0,cons<x,r>>>, 
               ?trans : (config<state-a,cons<sym1,l>,cons<x,r>>) -> () ) : ()
  ()
// stateB: if head==1 then write 1, shift the tape right, and continue in state B
fun b/b/trans( st : config<state-b,cons<x,l>,cons<sym1,r>>, 
               ?trans : (config<state-b,l,cons<x,cons<sym1,r>>>) -> () ) : ()
  ()
// stateC: if head==0 then write 1, shift the tape left, and continue in state B
fun c/b/trans( st : config<state-c,l,cons<sym0,cons<x,r>>>, 
               ?trans : (config<state-b,cons<sym1,l>,cons<x,r>>) -> () ) : ()
  ()
// stateC: if head==1 then write 1, do not move, and continue in state HALT
fun c/end/trans( st : config<state-c,l,cons<sym1,r>>, 
                 ?trans : () -> () ) : ()
  ()
fun inf/a/b/trans( st : config<state-a,inf,cons<sym0,r>>, 
                   ?trans : (config<state-a,cons<sym0,inf>,cons<sym0,r>>) -> () ) : ()
  ()
fun inf/a/c/trans( st : config<state-a,l,cons<sym1,inf>>, 
                   ?trans : (config<state-a,l,cons<sym1,cons<sym0,inf>>>) -> () ) : ()
  ()
fun inf/b/b/trans( st : config<state-b,inf,cons<sym1,r>>, 
                   ?trans : (config<state-b,cons<sym0,inf>,cons<sym1,r>>) -> () ) : ()
  ()
fun inf/b/a/trans( st : config<state-b,l,cons<sym0,inf>>, 
                   ?trans : (config<state-b,l,cons<sym0,cons<sym0,inf>>>) -> () ) : ()
  ()
fun inf/c/b/trans( st : config<state-c,l,cons<sym0,inf>>, 
                   ?trans : (config<state-c,l,cons<sym0,cons<sym0,inf>>>) -> () ) : ()
  ()
fun main()
  trans( start-config )
  println("done.")