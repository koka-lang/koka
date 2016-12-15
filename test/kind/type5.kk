type int
type app<f,a> { con App( app: f<a>) }
type id<a> { con Id(x:a) }
alias ok = app<id,int>
