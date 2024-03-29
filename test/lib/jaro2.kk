// --------------------------------------------------------
// Jaro distance using direct string slices
// --------------------------------------------------------
import std/data/array
import jaro

fun main() {
  jaro/run-tests( jaro-distance, fn(s,t) { jaro-winkler-distance(s,t) } )
}

fun jaro-winkler-distance( s : string, t : string, boost-threshold : float64 = 0.7 ) : float64 {
  val d = jaro-distance(s,t)
  if (d < boost-threshold) return d
  val l = common-prefix(s,t,4).count
  d + (l.float64 * 0.1 * (1.0 - d))
}

fun jaro-distance( s : string, t : string ) : float64 {
  if (s==t) return 1.0
  if (s.is-empty || t.is-empty) return 0.0
  val slen = s.count
  val tlen = t.count
  val lim  = max(0, max(slen,tlen)/2 - 1)
  val (mcount,tcount) = if (slen > tlen) then jaro-match(t,s,lim) else jaro-match(s,t,lim)
  if (mcount<=0) then 0.0 else {
    val mc = mcount.float64
    val tc = tcount.float64
    ((mc / slen.float64) + (mc / tlen.float64) + ((mc - tc) / mc)) / 3.0
  }
}


fun jaro-match( s : string, t : string, lim : int ) : (int,int) {
  try-default((0,0)) {
    val viewlen   = 2*lim + 1
    val matched   = array(viewlen,False)  // has a character in the view been matched before?
    var view : sslice := t.first.extend(lim)
    var matches   := 0;
    var transposes:= 0;
    var lastmatch := 0;
    var i := 0;                   // character index of `c`
    var j := 0;                   // character index of `d`
    s.foreach fn(c) {
      j := max(0, i - lim)        
      view.foreach-while fn(d) {
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
