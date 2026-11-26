// A reference in a heap given by a type parameter is not in `<local<h>|ioc>`.
fun poly( r : ref<h,int> ) : <read<h>,div> int
  var x := 1
  x := x + !r
  x
