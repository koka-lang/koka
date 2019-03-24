// polymorphic recursion
fun foo(xs:list<a>, n:int) : _ int
{
  if (n==0) then 1 else foo(single(xs),n - 1)
}

fun main() { foo(single(1),2) }