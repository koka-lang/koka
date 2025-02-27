
type integers
  SCons( i:int, rest:integers)

  lazy From(i : int, p : pad = Pad) ->
    SCons(i, From(i + 1))

  lazy FilterPrime(i : int, xs : integers) ->
    match xs
      SCons(x,xx) -> if (x%i) != 0 then SCons(x,FilterPrime(i,xx)) else FilterPrime(i,xx)

  lazy Sieve(xs : integers, p : pad = Pad) ->
    match xs
      SCons(x,xx) -> SCons(x,Sieve(FilterPrime(x,xx)))

fun sieve() : div integers
  Sieve(From(2))

tail fun take(s : integers, n : int) : div list<int>
  if n<=0 then Nil else match s
    SCons(x,xx) -> Cons(x,take(xx,n - 1))

pub fun main()
  sieve().take(1000)  // first 1000 primes