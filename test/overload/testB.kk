import testA

ref type foo
  Foo(x:int, y:int)

ref type foo2
  Foo2(x:int,y:int)

fun string/plus( x : string, y : string ) : string
  x ++ y

fun plus( x : bool, y : bool ) : bool
  x

fun test1()
  plus(1,2)

fun test2()
  plus("hi","there")

fun test3(x,y)
  int/plus(x,y)
