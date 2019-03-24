// polymorphic recursion on a dynamic closure
fun bar(ys,m) {
  fun foo(xs:list<a>, n:int) : _ int
  {
    if (n==0) then m else foo(single(xs),n - 1)
  }

  return foo(ys,m)
}

fun main() { print(bar([],1)) }