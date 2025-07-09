abstract type @hdiv<h::H,a::V,e::E>
  @Hdiv

noinline fun hread( r : ref<h,a>, ?@hdiv : @hdiv<h,a,e> ) : <read<h>|e> a
  impossible("hread")

noinline fun hwrite( r : ref<h,a>, x : a ) : write<h> a
  impossible("hwrite")

fun test1( ?@hdiv )
  val r = ref(1)
  hread(r)

fun test-landin()
  val r = ref(fn() ())
  fun f()
    hread(r)()
  hwrite(r,f)
  f()

fun test2()
  val r = ref(1)
  hread(r)

fun test3()
  test1()



