module string-buf

import std/time
import std/num/int32
import std/num/int64

extern import 
  c file "string-buf.c" 

// Working directly with strings by appending:
alias strbuf0 = string
fun strbuf0/empty(): strbuf0
  ""
fun strbuf0/append( buf : strbuf0, s : string ) : strbuf0
  buf ++ s
fun strbuf0/finalize( buf : strbuf0 ) : string
  buf

// List of chars, using constructor contexts:
alias strbuf1 = ctx<list<char>>
fun strbuf1/empty(): strbuf1
  ctx hole
fun list-char/append( buf : strbuf1, c : char ) : strbuf1
  buf ++ ctx Cons(c,hole)
fun list-char/append-chars( buf : strbuf1, cs : list<char> ) : strbuf1
  match cs
    Nil -> buf
    Cons(c,cc) -> buf.append(c).append-chars(cc)
fun strbuf1/append( buf : strbuf1, s : string ) : strbuf1
  buf.append-chars(list(s))
fun strbuf1/finalize( buf : strbuf1 ) : string
  (buf ++. Nil).string

// List of strings, using constructor contexts
alias strbuf2 = ctx<list<string>>
fun strbuf2/empty(): strbuf2
  ctx hole
fun strbuf2/append( buf : strbuf2, s : string ) : strbuf2
  buf ++ ctx Cons(s,hole)
fun strbuf2/finalize( buf : strbuf2 ) : string
  (buf ++. Nil).join

// List of strings, using final reversing:
alias strbuf3 = list<string>
fun strbuf3/empty(): strbuf3
  Nil
fun strbuf3/append( buf : strbuf3, s : string ) : strbuf3
  Cons(s,buf)
fun strbuf3/finalize( buf : strbuf3 ) : string
  buf.reverse-join


extern bsize( ^s : string ) : ssize_t
  c inline "kk_string_len_borrow(#1, kk_context())"

noinline val max-vec-size = 100
noinline val max-vec-size-int32 = max-vec-size.int32
extern int32/ssize_t( x : int32 ) : ssize_t
  c inline "#1"
extern ssize_t/(+)( x : ssize_t, y : ssize_t ) : ssize_t
  c inline "#1 + #2"
// Assign to an entry in a vector.
inline fip extern unsafe-assign( ^v : vector<a>, i : ssize_t, x : a ): total ()
  c "kk_vector_unsafe_assign_borrow"
  cs inline "(#1)[#2] = #3"
  js inline "(#1)[#2] = #3"
struct strbuf4
  cur: vector<string>
  buf: list<vector<string>>
  byte-size: ssize_t
  fill: int32
fun strbuf4/empty(): strbuf4
  Strbuf4(vector(max-vec-size, ""), [], 0.ssize_t, 0.int32)
fun strbuf4/append( sb : strbuf4, s : string ) : strbuf4
  match sb
    Strbuf4(cur, rst, byte-size, fill) | fill < max-vec-size-int32 -> 
      // Append to the current vector
      val b = cur.ensure-unique
      val new-size = byte-size + s.bsize
      b.unsafe-assign(fill.ssize_t, s)
      Strbuf4(b, rst, new-size, fill + 1.int32)
    Strbuf4(cur, rst, byte-size, _) -> // Fill == max-vec-size 
      val b = vector(max-vec-size, "")
      val new-size = byte-size + s.bsize
      b.unsafe-assign(0.ssize_t, s)
      Strbuf4(b, Cons(cur, rst), new-size, 1.int32)

fun strbuf4/finalize(sb: strbuf4): string 
  val Strbuf4(cur, buf, byte-size) = sb
  finalize-str(Cons(cur, buf).reverse, byte-size)
struct strbuf5
  cur: vector<string>
  buf: ctx<list<vector<string>>>
  byte-size: ssize_t
  fill: int32
fun strbuf5/empty(): strbuf5
  Strbuf5(vector(max-vec-size, ""), ctx hole, 0.ssize_t, 0.int32)
fun strbuf5/append( sb : strbuf5, s : string ) : strbuf5
  match sb
    Strbuf5(cur, rst, byte-size, fill) | fill < max-vec-size-int32 -> 
      // Append to the current vector
      val b = cur.ensure-unique
      val new-size = byte-size + s.bsize
      b.unsafe-assign(fill.ssize_t, s)
      Strbuf5(b, rst, new-size, fill + 1.int32)
    Strbuf5(cur, rst, byte-size, _) -> // Fill == max-vec-size
      val b = vector(max-vec-size, "")
      val new-size = byte-size + s.bsize
      b.unsafe-assign(0.ssize_t, s)
      Strbuf5(b, rst ++ ctx Cons(cur, hole), new-size, 1.int32)

fun strbuf5/finalize(sb: strbuf5): string 
  val Strbuf5(cur, rst, byte-size) = sb
  finalize-str(rst ++. Cons(cur, Nil), byte-size)

extern finalize-str( buf : list<vector<string>>, byte-size : ssize_t ) : string
  c "kk_strbuf_finalize"

// Using local variables:
fun bm-localvar(count: int, empty: a, ?append: (a, string) -> a, ?finalize: a -> string)
  val y =  
    var buf := empty
    for(1,count) fn(_)
      std/core/types/@byref(buf).update fn(b) 
        b.append(paris)
    buf
  y.finalize.keep.ignore

fun bm-localvar0(count) bm-localvar(count, strbuf0/empty())
fun bm-localvar1(count) bm-localvar(count, strbuf1/empty())
fun bm-localvar2(count) bm-localvar(count, strbuf2/empty())
fun bm-localvar3(count) bm-localvar(count, strbuf3/empty())
fun bm-localvar4(count) bm-localvar(count, strbuf4/empty())
fun bm-localvar5(count) bm-localvar(count, strbuf5/empty())

noinline val paris = "PARIS "

// Without local variables, but functional:
fun bm-functional(count, empty, ?append, ?finalize)
  fun func(remaining, buf)
    if ( remaining <= 0 ) then return buf
    func(remaining - 1, buf.append(paris))
  func(count, empty).finalize.keep.ignore

fun bm-functional0(count) bm-functional(count, strbuf0/empty())
fun bm-functional1(count) bm-functional(count, strbuf1/empty())
fun bm-functional2(count) bm-functional(count, strbuf2/empty())
fun bm-functional3(count) bm-functional(count, strbuf3/empty())
fun bm-functional4(count) bm-functional(count, strbuf4/empty())
fun bm-functional5(count) bm-functional(count, strbuf5/empty())

// Execute single benchmark multiple times:
fun benchmark(bm-func)
  val start = now-in()
  bm-func()
  val end = now-in()
  end - start

// Execute benchmarks:
fun benchmarks( count : int )
  // println("functional0: " ++ show(benchmark{bm-functional0(count)}))
  // println("functional1: " ++ show(benchmark{bm-functional1(count)}))
  println("functional2: " ++ show(benchmark{bm-functional2(count)}))
  println("functional3: " ++ show(benchmark{bm-functional3(count)}))
  println("functional4: " ++ show(benchmark{bm-functional4(count)}))
  println("functional5: " ++ show(benchmark{bm-functional5(count)}))
  // println("localvar0: " ++ show(benchmark{bm-localvar0(count)}))
  // println("localvar1: " ++ show(benchmark{bm-localvar1(count)}))
  println("localvar2: " ++ show(benchmark{bm-localvar2(count)}))
  println("localvar3: " ++ show(benchmark{bm-localvar3(count)}))
  println("localvar4: " ++ show(benchmark{bm-localvar4(count)}))
  println("localvar5: " ++ show(benchmark{bm-localvar5(count)}))

pub fun main()
  benchmarks(10000000)
  println("Double:")
  // benchmarks(200000)
