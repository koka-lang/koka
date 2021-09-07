effect foo {
    fun boo(x:int):int
}

fun test1(){
    print("In test1")
    with fun boo(x){x}
    boo(1)
}

fun f(): int {
    4 * 3
}

// Observe floating
// { boo(3) + open[...](f)(3) + open[...](f)(3) } ~~> { boo(3) + restrict[...](f() + f())}
fun g(): foo int {
    boo(3) + (f() + f())
}


fun main(){
    print("In main")
    test1()
}

// Compile this file with feature/open, other files with dev
// Below is the message for compiling main, expr of this file
// -----------------
// open floated
// ---------------
// public fun .expr : () -> console int
//   = fn<console>(){
//     val _ : ()
//           = std/core/prints("hi");
//     test/float/float1/test1();
//   };
// public fun .main : () -> console ()
//   = fn<console>(){
//     val s.75 : string
//           = std/core/types/.open(std/core/show)((interactive/.expr()));
//     std/core/printsln(s.75);
//   };