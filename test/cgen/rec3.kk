// polymorphic recursion on a dynamic closure
function bar(ys,m) {
  function foo(xs:list<a>, n:int) : _ int
  {
    if (n==0) then m else foo(single(xs),n - 1)
  }

  return foo(ys,m)
}

function main() { print(bar([],1)) }