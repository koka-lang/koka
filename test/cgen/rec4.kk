function foo(xs,n) {
    function bar(xs : list<int>) {
       match(xs) {
         Cons(_x,xx) -> bar(xx);
         Nil -> n
       }
    };
    bar(xs)
}


function main() { print(foo([],1)) }
