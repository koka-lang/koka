module javascript

function prematureReturn(xs: list<int>): bool {
  val x = match(xs) {
    Nil       -> return True // returning bool here..
    Cons(x,_) -> x           // while `x` is an `int`!
  }
  if(x == 0) True else False
}
 
function deepPatternMatching(xs : list<int>) : exn int {
  match(xs) {
    Nil -> 1
    Cons(_,Nil) -> 2
    Cons(_,Cons(_,Nil)) -> 3
    Cons(_,Cons(_,Cons(_,Nil))) -> 4
    Cons(_,Cons(_,Cons(_,Cons(_,Nil)))) -> 5
  }
}

function last2(xs : list<a>) : exn a {
  match(xs) {
    Cons(x,Nil) -> x
    Cons(_,zs)  -> last2(zs) 
  }
}

function main() {
  print(show(deepPatternMatching([1,2])))
}

/* optimizations to do: tco
   desired output: 

    function last2(xs) /* forall<a> (xs : list<a>) -> exn a */
    { tailcall: for(;;) {
      if(xs != null /* asCons */ && xs.tail == null /* singleton */ ) {
        return xs.head
      }
      if(xs != null /* asCons */) {
        var zs = xs.tail;
        {
          xs = zs;
          continue tailcall;
        }
      }
      return system_core.patternMatchError("test\u005Ccodegen\u005Cjavascript.kk(4, 3)","last2");;
    }}
*/

function inlineConditional1(x : bool) : int {
  if(x) 0 else 1
}

/* optimizations to do: inline with conditional expression
   desired output: 

   var inlineConditional1 = function(x) {
     return x?0:1;
   };
*/

function dontCheckLastCase(x: list<int>) : int {
  match(x) {
    Cons(_, _) -> 0
    Nil        -> 1
  }
}

/* optimizations to do: don't pattern match in last case, since core guarantees full coverage
   desired output: 

    var dontCheckLastCase = function(x /* system_core._list<int> */) {
      if(x[0] === 2) {
        return 0;
      }
      return 1;
    };
*/

struct person(age: int, name: string)

function matchOnStruct(person: person) : int {
  match(person)
  { Person(age, _) -> age
    _              -> 7
  }
}

/* optimizations to do: don't pattern match in last case, since core guarantees full coverage
   desired output: 

    function matchOnStruct(person) /* (person : person) -> int */
    {
      return person.age
    }
*/


function tcoWhile( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
{
  if (predicate()) {
    action()
    tcoWhile(predicate, action)
  }
}

function foo( i : int ) {
  if (i < 0) then return Lt
  elif (i > 0) then return Gt
  else Eq
}

function foo2( i : int ) {
  if (i < 0) then return Lt
  elif (i > 0) then return Gt
  else return Eq
}

function foo3( i : int ) {
  if (i < 0) then  Lt
  elif (i > 0) then  Gt
  else Eq
}

/* optimizations to do: tco 
   desired output: 

    { tailcall: for(;;) {
      if(predicate()) {
        action();;
        {
          predicate = predicate;
          action = action;
          continue tailcall;
        }
      } else {
        return system_core.__0unit_;;
      };
    }}

*/