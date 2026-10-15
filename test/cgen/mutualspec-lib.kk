// Support module for mutualspec.kk: mutually recursive functions that all
// carry a function-typed parameter.
module cgen/mutualspec-lib
import std/core/undiv

pub type node<a>
  NElem( tag : int, kids : list<node<a>> )
  NText( t : int )

pub type html<a>
  Elem( tag : int, children : list<html<a>> )
  Text( t : int )

pub fun build<msg>( h : html<msg>, emit : msg -> () ) : <div,exn> node<msg>
  match h
    Text(t) -> NText(t)
    Elem(tag, kids) ->
      NElem(tag, kids.map( fn(k) k.pretend-decreasing.build(emit) ))

pub fun patch<msg>( old : node<msg>, new : html<msg>, emit : msg -> () ) : <div,exn> node<msg>
  match (old, new)
    (NText(_), Text(t2)) -> NText(t2)
    (NElem(t1, kids1), Elem(t2, kids2)) | t1 == t2 ->
      NElem(t1, patch-kids(kids1, kids2, emit))
    (_, n) -> n.build(emit)

fun patch-kids<msg>( olds : list<node<msg>>, news : list<html<msg>>, emit : msg -> () ) : <div,exn> list<node<msg>>
  match (olds, news)
    (Nil, Nil) -> Nil
    (Cons(o, os), Cons(n, ns)) -> Cons( patch(o.pretend-decreasing, n, emit), patch-kids(os, ns, emit) )
    (Cons(_, os), Nil) -> patch-kids(os, Nil, emit)
    (Nil, Cons(n, ns)) -> Cons( n.build(emit), patch-kids(Nil, ns, emit) )
