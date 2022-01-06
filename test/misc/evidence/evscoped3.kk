pub module resolve-problem

import model-using-effect

// import std/num/ddouble // Commenting this line out hides
// (<) : ddouble -> ddouble -> bool and produces an error

fun not-handling(a: number, b: number){
     a < b
}
