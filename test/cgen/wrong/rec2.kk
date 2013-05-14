// polymorphic recursion but without a type signature: should be rejected???
fun foo(xs,n)
{
  if (n==0) then 1 else foo(single(xs),n - 1)
}

fun main() { foo(single(1),2) }