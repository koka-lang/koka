function main()
{
  val s = "hello effectful world of 効果! ";
  println( s.list.show-list(fun(c) {show-hex(c.int)}) );
  println(s)
}
