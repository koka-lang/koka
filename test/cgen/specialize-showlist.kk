fun show(xs : list<(int,list<int>)>) : string 
  xs.show-list fn(x) 
    "(state=" ++ x.fst.show ++ ", " ++ x.snd.show ++ ")"

fun main() 
  val xss = [(1,[1,2]),(2,[3,4,5]),(3,[])] 
  xss.show.println
