import std/num/int32

type list-int
  NilInt
  ConsInt(head : int, tail : list-int)

fun test-head( xs : ctx<list-int>) : int
  match xs
    @Cctx( ConsInt(x, _), _holeptr ) -> x
    _ -> 0

fun test-headx( xs : cctx<list-int,int>) : int
  match xs
    @Cctx( ConsInt(_,_), _holeptr ) -> 1
    _ -> 0


fun test1()
  val xs = ctx ConsInt(42, _)
  match xs
    @Cctx( ConsInt(x, _), _holeptr ) -> x


fun test-wrong2() // : exn ctx<list-int>
  val xs = ctx ConsInt(42, _)
  match xs
    @Cctx( ConsInt(_x, rest), _holeptr ) -> rest       // is rest list-int or ctx<list-int>  ?


fun test2a() // : exn ctx<list-int>
  val xs = ctx ConsInt(42, _) // ConsInt(43, _) )
  // match xs
  //   ctx ConsInt(_,ctx rest) -> rest
  // ~>
  match xs
    @Cctx( parent as ConsInt(_, valrest), holeptr ) | cctx/field-is-ctx(parent,1.int32) ->
      val rest = unsafe-Cctx(valrest,holeptr)       // is rest list-int or ctx<list-int>  ?
      rest

fun test2b()
  val xs = ctx ConsInt(42, _)
  match xs
    @Cctx( parent as ConsInt(x, _hole), holeptr ) | cctx/field-is-hole(parent,1.int32,holeptr) -> x       // is rest list-int or ctx<list-int>  ?


fun test2c() // : exn ctx<list-int>
  // val xs = ctx ConsInt(42, ConsInt(43, _) )
  val xs = ctx ConsInt(42, ConsInt(43, ConsInt(_,NilInt)))
  // match xs
  //   ctx ConsInt(_,ConsInt(_,ctx rest)) -> rest
  // ~>
  match xs
    @Cctx( parent1 as ConsInt(_, parent2 as ConsInt(_, valrest)), holeptr)
          | cctx/field-is-ctx(parent1,1.int32) && cctx/field-is-ctx(parent2,1.int32) ->
            val rest = unsafe-Cctx(valrest,holeptr)       // is rest list-int or ctx<list-int>  ?
            rest

pub fun main()
  test1().println
  test2a().test-head.println
  test2b().println
  test2c().test-headx.println
