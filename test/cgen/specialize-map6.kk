
fun test(xs : list<_>, f, g) : total list<_> {
  xs.map( fn(x){ f(g(x))} )   // free variable  
}

pub fun main() {
  list(1,10).test(fn(x){x*2},inc).sum.println
}
