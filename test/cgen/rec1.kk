function foo(x : int ) : div int {
  if (x==0) then foo(True) else foo(x - 1);
}

function foo(b : bool ) : div int {
  if (b) then 1 else foo(1)
}

function main() { foo(10) }
