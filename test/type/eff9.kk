type foo :: X

external inline onevent : forall<e> ( () -> e () ) -> <foo|e> ()
{ inline "" }

fun f() : <foo,foo> ()
{ fun g() : foo ()
  {
    return ()
  }
  onevent(g)
}
