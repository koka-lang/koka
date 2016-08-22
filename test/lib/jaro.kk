// --------------------------------------------------------
// Jaro distance
// --------------------------------------------------------
import std/array

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