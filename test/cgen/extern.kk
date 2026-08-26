import std/core/cextern
import std/num/int32

extern import
  c file "extern.h"

pub type c-struct;
pub alias kstructo = owned-c<c-struct>;
pub alias kstructoa = owned-c<c-array<c-struct>>;
pub alias kstructb<s> = borrowed-c<s,c-struct>;

pub extern size-of(c: c-null<c-struct>): int32
  c inline "sizeof(c_struct_t)"

inline extern ptr/xint(s: c-pointer<c-struct>): int32
  c inline "((c_struct_t*)#1)->xint"
 
inline extern ptr/set-xint(s: c-pointer<c-struct>, x: int32): ()
  c inline "((c_struct_t*)#1)->xint = #2"

pub fun kstructo(): kstructo
  malloc()

pub fun kstructoc(): kstructo
  malloc-c()

pub fun kstruct-array(n: int): kstructoa
  malloc(n.int32)

pub inline fun kstruct/xint(^s: kstructo): int32
  s.with-ptr(xint)

pub inline fun kstructb/xint(^s: kstructb<s>): int32
  s.with-ptr(xint)

pub fun set-xintf(x: int32): ((c-pointer<c-struct>) -> ())
  fn(p) set-xint(p, x)

pub inline fun kstruct/set-xint(^s: kstructo, x: int32): ()
  s.with-ptr(set-xintf(x))

pub inline fun kstructb/set-xint(^s: kstructb<s>, x: int32): ()
  s.with-ptr(set-xintf(x))

fun main()
  val s = kstructo()
  set-xint(s, 42.int32)
  println(xint(s))

  val s2 = kstructoc()
  set-xint(s2, 43.int32)
  println(xint(s2))

  val ss = kstruct-array(10)
  for(10) fn(i)
    ss.with-ptr(i.ssize_t) fn(b)
      b.set-xint(i.int32)
  for(10) fn(i)
    ss.with-ptr(i.ssize_t) fn(b)
      println(b.xint)
