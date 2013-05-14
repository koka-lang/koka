// partially applied type synonym
type foo
type ap<f,a> { Ap( :f<a>) }
alias id<a> = a

alias wrong = ap<id,foo>
