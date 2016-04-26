// polymorphic recursion
function foo(xs:list<a>, n:int) : _ int
{
  if (n==0) then 1 else foo(single(xs),n - 1)
}

function main() { foo(single(1),2) }