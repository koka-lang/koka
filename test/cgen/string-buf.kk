import std/time

// Working directly with strings by appending:
alias strbuf0 = string
fun strbuf0/empty(): strbuf0
  ""
pub fun strbuf0/append( buf : strbuf0, s : string ) : strbuf0
  buf ++ s
pub fun strbuf0/finalize( buf : strbuf0 ) : string
  buf

// List of chars, using constructor contexts:
alias strbuf1 = ctx<list<char>>
fun strbuf1/empty(): strbuf1
  ctx hole
pub fun list-char/append( buf : strbuf1, c : char ) : strbuf1
  buf ++ ctx Cons(c,hole)
pub fun list-char/append-chars( buf : strbuf1, cs : list<char> ) : strbuf1
  match cs
    Nil -> buf
    Cons(c,cc) -> buf.append(c).append-chars(cc)
pub fun strbuf1/append( buf : strbuf1, s : string ) : strbuf1
  buf.append-chars(list(s))
pub fun strbuf1/finalize( buf : strbuf1 ) : string
  (buf ++. Nil).string

// List of strings, using constructor contexts
alias strbuf2 = ctx<list<string>>
fun strbuf2/empty(): strbuf2
  ctx hole
pub fun strbuf2/append( buf : strbuf2, s : string ) : strbuf2
  buf ++ ctx Cons(s,hole)
pub fun strbuf2/finalize( buf : strbuf2 ) : string
  (buf ++. Nil).join

// List of strings, using final reversing:
alias strbuf3 = list<string>
fun strbuf3/empty(): strbuf3
  Nil
pub fun strbuf3/append( buf : strbuf3, s : string ) : strbuf3
  Cons(s,buf)
pub fun strbuf3/finalize( buf : strbuf3 ) : string
  buf.reverse-join

// Using local variables:
pub fun bm-localvar(count: int, empty: a, ?append: (a, string) -> a, ?finalize: a -> string)
  val y =  
    var buf := empty
    for(1,count) fn(_)
      std/core/types/@byref(buf).update fn(b) 
        b.append("PARIS ")
    buf
  y.finalize.keep.ignore

pub fun bm-localvar0(count) bm-localvar(count, strbuf0/empty())
pub fun bm-localvar1(count) bm-localvar(count, strbuf1/empty())
pub fun bm-localvar2(count) bm-localvar(count, strbuf2/empty())
pub fun bm-localvar3(count) bm-localvar(count, strbuf3/empty())


// Without local variables, but functional:
pub fun bm-functional(count, empty, ?append, ?finalize)
  fun func(remaining, buf)
    if ( remaining <= 0 ) then return buf
    func(remaining - 1, buf.append("PARIS "))
  func(count, empty).finalize.keep.ignore

pub fun bm-functional0(count) bm-functional(count, strbuf0/empty())
pub fun bm-functional1(count) bm-functional(count, strbuf1/empty())
pub fun bm-functional2(count) bm-functional(count, strbuf2/empty())
pub fun bm-functional3(count) bm-functional(count, strbuf3/empty())

// Execute single benchmark multiple times:
pub fun benchmark(bm-func)
  val start = now-in()
  bm-func()
  val end = now-in()
  end - start

// Execute benchmarks:
pub fun benchmarks( count : int )
  println("functional0: " ++ show(benchmark{bm-functional0(count)}))
  println("functional1: " ++ show(benchmark{bm-functional1(count)}))
  println("functional2: " ++ show(benchmark{bm-functional2(count)}))
  println("functional3: " ++ show(benchmark{bm-functional3(count)}))
  println("localvar0: " ++ show(benchmark{bm-localvar0(count)}))
  println("localvar1: " ++ show(benchmark{bm-localvar1(count)}))
  println("localvar2: " ++ show(benchmark{bm-localvar2(count)}))
  println("localvar3: " ++ show(benchmark{bm-localvar3(count)}))

pub fun main()
  benchmarks(100000)
  println("Double:")
  benchmarks(200000)