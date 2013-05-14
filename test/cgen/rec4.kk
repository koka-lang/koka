fun foo(xs,n) {
    fun bar(xs : list<int>) {
       match(xs) {
         Cons(_x,xx) -> bar(xx);
         Nil -> n
       }
    };
    bar(xs)
}


fun main() { print(foo([],1)) }
