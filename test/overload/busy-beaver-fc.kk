// This example encodes a 3-state busy beaver: <https://en.wikipedia.org/wiki/Turing_machine_examples#3-state_Busy_Beaver>
// program in a Turing machine encoded in the type system.

// -------------------------------------------------------------
// | current |    state A    |    state B    |    state C      |
// | symbol  |write move next|write move next|write move next  |
// -------------------------------------------------------------
// | 0	     | 1	   R	  B	 | 1	   L	  A	 | 1	   L	  B    |
// | 1	     | 1	   L	  C	 | 1	   R	  B	 | 1	   N	  HALT |
// -------------------------------------------------------------

// symbols 0 and 1
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

// all states can expand the `inf` tape if that state has a transition which shifts the first symbol off the inf tape.
fun inf-l/trans( st : config<s,inf,cons<h,r>>, ?a/trans : (config<s,cons<sym0,inf>,cons<h,r>>, ?transition: (config<s1,inf,r2>) -> ()) -> (), 
                                                    ?b/trans: (config<s,cons<sym0,inf>,cons<h,r>>) -> ()) : ()
  ()
// all states can expand the `inf` tape if that state has a transition which shifts the first symbol off the inf tape.
fun inf-r/trans( st : config<s,l,cons<h,inf>>, ?a/trans : (config<s,l,cons<h,cons<sym0,inf>>>, ?transition: (config<s1,l2,cons<h2,inf>>) -> ()) -> (), 
                                                    ?b/trans: (config<s,l,cons<h,cons<sym0,inf>>>) -> () ) : ()
  ()
fun main()
  trans( start-config )
  println("done.")