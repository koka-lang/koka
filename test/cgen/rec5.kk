fun foo(xs,n) {
    fun bar(xs) {
       match(xs) {
         // cons(x,xx) -> bar(xx);
         Nil -> n
       }
    };
    bar(xs)
}


fun main() { print(foo([],1)) }