// --------------------------------------------------------
// Jaro distance
// --------------------------------------------------------
import std/array

val jaro-tests : list<(string,string,double,double)> = 
[("abc","abc",1.0,1.0),
 ("","",1.0,1.0),
 ("","abc",0.0,0.0),
 ("abc","",0.0,0.0),
 ("martha","marhta",0.944,0.961),
 ("dixon","dicksonx",0.767,0.814),
 ("jones","johnson",0.790,0.832),
 ("abcvwxyz","cabvwxyz",0.958,1.1),
 ("abcawxyz","bcawxyz",0.91,1.1),
 ("dunningham","cunnigham",1.1,0.90),
 ("abroms","abrams",1.1,0.92),
 ("lampley","campley",1.1,0.9),
 ("jonathon","jonathan",1.1,0.95),
 ("jeraldine","geraldine",1.1,0.93),
]

fun run-tests() {
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
  val ss = s.first.extend(3).string.list
  val ts = t.first.extend(3).string.list
  val l  = zip(ss,ts).take-while(fun(x) { x.fst==x.snd }).length
  d + (l.double * 0.1 * (1.0 - d))
}


fun jaro-distance( s : string, t : string ) : double {
  if (s==t) return 1.0
  if (s.empty? || t.empty?) return 0.0
  val m = s.count
  val n = t.count
  val lim = max(m,n)/2 - 1
  if (m > n) then jaro-match(t,n,s,m,lim) else jaro-match(s,m,t,n,lim)
}


fun jaro-match( s : string, slen: int, t : string, tlen : int, lim : int ) : double {
  on-exn(0.0) {
    val viewlen   = 2*lim + 1
    val matched   = array(viewlen,False)
    var view      := t.first.extend(lim)
    var viewstart := 0;
    var matches   := 0;
    var transposes:= 0;
    var lastmatch := 0;  
    s.foreach-indexed fun(c,i) {
      view.foreach-while-indexed fun(d,viewi) {
        val j = viewstart + viewi
        if (c != d || matched[j % viewlen]) return Nothing
        matched[j % viewlen] := True
        matches := matches + 1
        if (lastmatch > j) transposes := transposes + 1
        lastmatch := j
        Just(())
      }
      if (i < lim) {
        view := view.extend(1)
      }
      else {
        matched[viewstart % viewlen] := False
        viewstart := viewstart + 1
        view := view.advance(1)
      }
    }
    if (matches==0) return 0.0
    val md = matches.double
    val td = transposes.double
    ((md / slen.double) + (md / tlen.double) + ((md - td) / md)) / 3.0
  }
}


/*
fun jaro-proximity( s : string, t : string ) : exn double {
  if (s==t) return 1.0
  if (s.empty? || t.empty?) return 0.0
  val v = s.vector
  val w = t.vector
  val lim = max(v.length,w.length)/2 - 1
  if (v.length > w.length) then jaro-match(w,v,lim) else jaro-match(v,w,lim)
}

fun jaro-match( v : vector<char>, w : vector<char>, lim : int ) : exn double {
  val matched = array(w.length,False)
  var transposes := 0
  var matches    := 0
  var lastmatch  := 0
  v.foreach-indexed fun(c,i) {
    val start = max(0,i - lim)
    val end   = min(w.length - 1, i + lim)
    for-while(start,end) fun(j) {
      if (matched[j]) Nothing
      elif (w[j] != c) Nothing
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
  if (matches==0) return 0.0
  val m = matches.double
  val t = transposes.double
  ((m / v.length.double) + (m / w.length.double) + ((m - t) / m)) / 3.0
}
*/