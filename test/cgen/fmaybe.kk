import std/num/int32

type xmaybe<a>
  XJust( value : a )
  XNothing

value type vmaybe<a>
  VJust( value : a )
  VNothing

fun default(x,def) 
  match x
    VJust(v) -> v
    VNothing -> def

reference type imaybe
  IJust( value : int32 )
  INothing

reference type lmaybe<a>
  LJust( value : list<a> )
  LNothing  

type xlist<a> 
  XNil
  XCons( hd : a, tl : xlist<a>)  

fun ignorex(x)
  "(ignore)"


fun ishow( i : int ) 
  i.show 

noinline fun mshow( mb : xmaybe<a>, vshow : a -> string ) : string 
  match mb
    XJust(v) -> "XJust(" ++ vshow(v) ++ ")"
    XNothing -> "XNothing"

noinline fun imshow(mb)
  match mb
    XJust(i) -> "XJusti(" ++ ishow(i) ++ ")"
    XNothing -> "XNothingi"

noinline fun jmshow(mb)
  match mb
    IJust(i) -> "IJust( " ++ show(i) ++ ")"
    INothing -> "INothing"

fun main() 
  mshow( XNothing, ignorex ).println
  mshow( XJust(1), show ).println
  imshow( XJust(1) ).println
  mshow( XJust(XNothing), fn(v) mshow(v,ishow) ).println
  mshow( XJust(XJust(2)), fn(v) mshow(v,ishow) ).println
