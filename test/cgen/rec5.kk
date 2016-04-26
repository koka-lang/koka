function foo(xs,n) {
    function bar(xs) {
       match(xs) {
         // cons(x,xx) -> bar(xx);
         Nil -> n
       }
    };
    bar(xs)
}


function main() { print(foo([],1)) }