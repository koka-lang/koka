// --------------------------------------------------------
// Jaro distance with vector allocation
// --------------------------------------------------------
import std/data/array
import jaro

fun main() {
  jaro/run-tests( jaro-distance, fun(s,t) { jaro-winkler-distance(s,t) } ) 
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
    v.foreach-indexed fun(c,i) {
      val start = max(0,i - lim)
      val end   = min(w.length - 1, i + lim)
      for-while(start,end) fun(j) {
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
