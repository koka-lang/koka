// The current C-sharp translation does not support 'nested' polymorphic higher kinds
function foo( xs : list<c<a>> ) { 1 }

function main() = foo(single(single(1)))