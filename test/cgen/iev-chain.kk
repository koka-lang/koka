// implicit chains: one resolution can require several hdiv constraints,
// nested at depth through implicit parameters of the chosen candidates
fun refs/add( r1 : ref<h,int>, r2 : ref<h,int>, ?hdiv : hdiv<h,int,e> ) : <read<h>|e> int
  (!r1) + (!r2)

fun refs/sum3( r1 : ref<h,int>, r2 : ref<h,int>, r3 : ref<h,int>,
               add : (ref<h,int>, ref<h,int>) -> <read<h>|e> int ) : <read<h>|e> int
  add(r1,r2) + !r3

pub fun main()
  val a = ref(1)
  val b = ref(2)
  val c = ref(3)
  println( refs/sum3( a, b, c, refs/add ) )
