struct counter<h>( count : ref<h,int> )


function test() {
  val c = Counter( count = ref(0) )
  c.count := 1
  print(!c.count)
}
