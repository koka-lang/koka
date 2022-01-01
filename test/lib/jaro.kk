// --------------------------------------------------------
// Jaro distance from the std/string library
// --------------------------------------------------------
module jaro

import std/data/array

pub fun main() {
  run-tests(jaro-distance,fn(s,t){ jaro-winkler-distance(s,t) })
}

pub fun run-tests(jaro,jarow) {
  jaro-tests.foreach fn(test) {
    val (s,t,jscore,jwscore) = test
    println(s.show ++ " - " ++ t.show)
    print-test( jaro(s,t), jscore, "jaro")
    print-test( jarow(s,t), jwscore, "jw  ")
    println("")
  }
}

fun print-test( score : double, expect : double, msg : string ) {
  val sscore  = score.show-fixed(2)
  val sexpect = expect.show-fixed(2)
  println(msg ++ ": " ++ sscore ++ ": " ++ (if (expect > 1.0 || sscore==sexpect) then "success" else "FAILED, expecting: " ++ sexpect))
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



fun jaro-winkler-distance( s : string, t : string, boost-threshold : double = 0.7 ) : double {
  val d = jaro-distance(s,t)
  if (d < boost-threshold) return d
  val l = common-prefix(s,t,4).count
  d + (l.double * 0.1 * (1.0 - d))
}

fun jaro-distance( s : string, t : string ) : double {
  if (s==t) return 1.0
  if (s.is-empty || t.is-empty) return 0.0
  val v = s.vector
  val w = t.vector
  val lim = max(0,max(v.length,w.length)/2 - 1)
  val (mcount,tcount) = if (v.length > w.length) then jaro-match(w,v,lim) else jaro-match(v,w,lim)
  if (mcount<=0) then 0.0 else {
    val mc = mcount.double
    val tc = tcount.double
    ((mc / v.length.double) + (mc / w.length.double) + ((mc - tc) / mc)) / 3.0
  }
}

fun jaro-match( v : vector<char>, w : vector<char>, lim : int ) : (int,int) {
  try-default((0,0)) {
    val matched = array(w.length,False)
    var transposes := 0
    var matches    := 0
    var lastmatch  := 0
    v.foreach-indexed fn(c,i) {
      val start = max(0,i - lim)
      val end   = min(w.length - 1, i + lim)
      for-while(start,end) fn(j) {
        if (w[j] != c || matched[j]) Nothing
        else {
          matched[j] := True
          matches := matches + 1
          if (lastmatch > j) transposes := transposes + 1
          lastmatch := j
          Just(())
        }
      }
      ()
    }
    (matches,transposes)
  }
}
