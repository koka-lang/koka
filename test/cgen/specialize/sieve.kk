fun sieve(xs : list<int>, max : int ) : div list<int>
  match xs 
    Nil        -> Nil
    Cons(x,xx) -> if x*x > max then xx 
                    else Cons(x, xx.filter(fn(y) y%x != 0).sieve(max))

fun gen-primes(n : int) : div list<int>
  list(2,n).sieve(n)

fun test(n)
  gen-primes(n).foreach(println)

fun main()
  test(100)  