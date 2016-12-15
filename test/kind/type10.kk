// Test if type synonym variables are correctly generalized in constructors.
alias id<a> = a
alias ap<f,a> = f<a>
type list<a> { con Single(:a) }
type yy<a> { con Yy( :ap<list,a> ) }
type zz<a> { con Zz( :ap<list,id<a>> ) }
