// illegal recursive call with more arguments
// used to trigger bug in divergent analysis

function rfix(f) { f( fun(x) { rfix(f,x) }) }