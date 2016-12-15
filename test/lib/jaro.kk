// --------------------------------------------------------
// Jaro distance from the std/string library
// --------------------------------------------------------
module jaro

import std/text/string

public fun main() {
  run-tests(jaro-distance,fun(s,t){ jaro-winkler-distance(s,t) })
}

public fun run-tests(jaro,jarow) {
  jaro-tests.foreach fun(test) {
    val (s,t,jscore,jwscore) = test
    println(s.show + " - " + t.show)
    print-test( jaro(s,t), jscore, "jaro")
    print-test( jarow(s,t), jwscore, "jw  ")
    println("")
  }
}

fun print-test( score : double, expect : double, msg : string ) {
  val sscore  = score.show-fixed(2)
  val sexpect = expect.show-fixed(2)
  println(msg + ": " + sscore + ": " + (if (expect > 1.0 || sscore==sexpect) then "success" else "FAILED, expecting: " + sexpect))
}

val jaro-tests : list<(string,string,double,double)> = 
[("abc","abc",1.0,1.0),
 ("","",1.0,1.0),
 ("","abc",0.0,0.0),
 ("abc","",0.0,0.0),
 ("a","a",1.0,1.0),
 ("a","bc",0.0,0.0),
 ("martha","marhta",0.944,0.961),
 ("dixon","dicksonx",0.767,0.814),
 ("jones","johnson",0.790,0.832),
 ("abcvwxyz","cabvwxyz",0.958,0.96),
 ("abcawxyz","bcawxyz",0.91,0.91),
 ("dunningham","cunnigham",0.90,0.90),
 ("abroms","abrams",0.89,0.92),
 ("lampley","campley",0.9,0.9),
 ("jonathon","jonathan",0.92,0.95),
 ("jeraldine","geraldine",0.93,0.93),
 ("dwayne","duane",0.82,0.84)
]

