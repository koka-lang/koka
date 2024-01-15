module std/core/order

import std/core/types
import std/core/hnd
import std/core/int

pub fip fun int( x : order ) : int
  match x
    Lt -> -1
    Eq -> 0
    Gt -> 1

pub fun(==)( x : order, y : order ) : bool
  x.int == y.int

pub fun(!=)( x : order, y : order ) : bool
  x.int != y.int

pub fun(>=)( x : order, y : order ) : bool
  x.int >= y.int

pub fun(<=)( x : order, y : order ) : bool
  x.int <= y.int

pub fun(>)( x : order, y : order ) : bool
  x.int > y.int

pub fun(<)( x : order, y : order ) : bool
  x.int < y.int
