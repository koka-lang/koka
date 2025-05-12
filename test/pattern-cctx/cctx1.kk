import std/num/int32

type list-int
  NilInt
  ConsInt(head : int, tail : list-int)

fun test-head( xs : ctx<list-int>) : int
  match xs
    _ | cctx/is-empty(xs) -> 0              // we must test for the empty context first
    @Cctx( ConsInt(x, _), _holeptr ) -> x
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
  val xs = ctx ConsInt(42, ConsInt(43, _) )
  match xs
    @Cctx( parent as ConsInt(_x, rest), holeptr ) | cctx/field-is-ctx(parent,1.int32) ->
      if cctx/field-is-hole(parent,1.int32,holeptr)
        then ctx _
        else cctx/unsafe-cast-ctx(xs,rest)       // is rest list-int or ctx<list-int>  ?

fun test2b()
  val xs = ctx ConsInt(42, _)
  match xs
    @Cctx( parent as ConsInt(x, _hole), holeptr ) | cctx/field-is-hole(parent,1.int32,holeptr) -> x       // is rest list-int or ctx<list-int>  ?

