// polymorphic recursion but without a type signature: should be rejected???
function foo(xs,n)
{
  if (n==0) then 1 else foo(single(xs),n - 1)
}

function main() { foo(single(1),2) }