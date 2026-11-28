module synpriv-lib

// Helper for synpriv.kk: the private `pairs` appears in a public signature, so
// the .kki must declare it for an importer to expand it.
alias pairs<v> = list<(v,v)>

pub fun swap-all(xs: pairs<int>): pairs<int>
  xs.map(fn((a,b)) (b,a))

pub fun main()
  swap-all([(1,2)]).map(fn((a,b)) a - b).sum.println
