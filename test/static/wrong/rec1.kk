// illegal recursive call with more arguments
// used to trigger bug in divergent analysis

fun rfix(f) { f( fn(x){ rfix(f,x) }) }