type foo :: X

external inline onevent : forall<e> ( () -> e () ) -> <foo|e> ()
{ inline "" }

function f() : <foo,foo> ()
{ function g() : foo ()
  {
    return ()
  }
  onevent(g)
}
