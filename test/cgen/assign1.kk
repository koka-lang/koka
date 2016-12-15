struct counter<h>( count : ref<h,int> )

function count( c : counter<h>, assigned : int) : write<h> () { 
  c.assign1/count.set(assigned) 
}

function main() {
  val c = Counter( count = ref(0) )
  c.count := 1
  c.count.set := 2
  print(!(c.count))
}
