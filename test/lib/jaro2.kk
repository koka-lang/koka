// --------------------------------------------------------
// Jaro distance
// --------------------------------------------------------
import std/array

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
]

fun main() {
  jaro-tests.foreach fun(test) {
    val (s,t,_,_) = test
    println(s.show + " - " + t.show)
    //test.print-test(jaro-proximity, jaro-winkler-proximity, "1") 
    test.print-test(jaro-distance, jaro-winkler-distance, "2")
    println("") 
  }
}

fun print-test(test : (string,string,double,double),f,g : (string,string,?double) -> exn double,msg) {
  val (s,t,d1,d2) = test
  val score1  = f(s,t).show-fixed(2)
  val score2  = g(s,t).show-fixed(2)
  val expect1 = d1.show-fixed(2) 
  val expect2 = d2.show-fixed(2)
  println(msg + ": jaro: " + score1 + " - " + expect1 + ": " + (if (d1 > 1.0 || score1==expect1) then "success" else "FAILED"))
  println(msg + ": jw  : " + score2 + " - " + expect2 + ": " + (if (d2 > 1.0 || score2==expect2) then "success" else "FAILED"))
}


fun jaro-winkler-distance( s : string, t : string, boost-threshold : double = 0.7 ) : double {
  val d = jaro-distance(s,t)
  if (d < boost-threshold) return d
  val l = common-prefix(s,t,4).count
  d + (l.double * 0.1 * (1.0 - d))
}

fun jaro-distance( s : string, t : string ) : double {
  if (s==t) return 1.0
  if (s.empty? || t.empty?) return 0.0
  val slen = s.count
  val tlen = t.count
  val lim  = max(0, max(slen,tlen)/2 - 1)
  val (mcount,tcount) = if (slen > tlen) then jaro-match(t,s,lim) else jaro-match(s,t,lim)
  if (mcount<=0) then 0.0 else {
    val mc = mcount.double
    val tc = tcount.double
    ((mc / slen.double) + (mc / tlen.double) + ((mc - tc) / mc)) / 3.0
  }
}


fun jaro-match( s : string, t : string, lim : int ) : (int,int) {
  on-exn((0,0)) {
    val viewlen   = 2*lim + 1
    val matched   = array(viewlen,False)  // has a character in the view been matched before?
    var view      := t.first.extend(lim)
    var matches   := 0; 
    var transposes:= 0;
    var lastmatch := 0;
    var i := 0;                   // character index of `c`
    s.foreach fun(c) {
      var j := max(0, i - lim)    // character index of `d` 
      view.foreach-while fun(d) {
        if (c != d || matched[j % viewlen]) {
          j := j + 1
          Nothing
        }
        else {
          matched[j % viewlen] := True
          matches := matches + 1
          if (lastmatch > j) transposes := transposes + 1
          lastmatch := j
          Just(())
        }
      }
      // extend or advance the view forward
      if (i < lim) {
        view := view.extend(1)
      }
      else {
        matched[(i - lim) % viewlen] := False  
        view := view.advance(1)
      }
      i := i+1
    }
    (matches,transposes)
  }
}