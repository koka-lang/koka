// partially applied type synonym
type foo

alias id<a> = a
alias ap<f,a> = f<a>

alias ok = ap<id,foo>