effect fun op1() : int
effect control op2() : (() -> op1 int)

fun f2( k: (() -> op1 int) -> op1 int ): op1 int {
  k {       // () -> op1 ()
    k {     // () -> op1 ()
      op1() // | op1
    }
  }
}


fun main() : total int {
    with fun op1(){ 42 }
    with control op2(){ f2(resume) }    // resume: (() -> op1 ()) -> op1 ()
    // this will duplicate the handler for op2 if it would be accepted
    // op2     :: () -> <op2|e1> (() -> op1 ())
    // op2()   :: () -> op1 ()  | <op2|e1>
    // op2()() :: _if_ we wrongly "open", then
    //    () -> op1 () goes to () -> <op1|e2> ()  
    // and then e1 ~= <op1> and e2 ~= <op2>  
    op2()()
}

// Evidence passing (ICFP 2021)
// *typed* lambda^eff ~= evidence passing lambda
// lambda^eff untyped  (but doesn't evaluate with evidence passing??)
/*
handler h_op1 . handler h_op2 . [] () . perform op2 ()
<< op1, op2 >>                .  ...
-->
handler op1 . handler op2 . [] () . ({ k { perform op1 () }) })
-->
handler op1 . handler op2 . k { perform op1 () }
-->
handler op1 . handler h_op2 . handler h_op2 . [] () . ({ perform op1 () })
                <op1>             <op1>
<<op1>>     .  <<op1,op2>>  .  
-->
handler op1 . handler op2 . handler op2 . perform op1 ()
-->
handler op1 . handler op2 . handler op2 . ()
-->*
()

*/
