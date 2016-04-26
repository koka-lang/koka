function foo(xs,n) {
    function bar(xs) {
       match(xs) {
         Cons(_x,xx) -> bar(xx);
         Nil -> n
       }
    };
    bar(xs)
}


function main() { print(foo([],1)) }
