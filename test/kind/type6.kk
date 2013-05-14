type int
alias id<a :: V -> V >  = a<int>
type list<a> { con Single(x:a) }
alias ok = id<list>
