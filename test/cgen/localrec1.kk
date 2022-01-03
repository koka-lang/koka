/*
fun my-show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
{
  "[" ++ xs.map(show-elem).join(",") ++ "]"
}
*/

fun main(x : int) 
  fun f(i:int) 
    fun g(j:int) 
      fun h(k:int)
         if k==0 then x+k else h(k - 1)
      if j==0 then h(i+x) else g(j - 1)  
    if i==x then g(i) else f(i - 1)
  f(x+1)
