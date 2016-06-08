module caesar

public fun encode(s : string, shift : int = 3) {
  fun encode-uni( u : unicode ) {
    val base = u - "a".code
    if (base < 0 || base > 25) return u
    val rot  = (base + shift) % 26
    rot + "a".code
  }
  s.map(encode-uni)
}

public fun caesar( s : string ) {
  s.encode(3)
}

// The letter frequency table for English
val english = [8.2,1.5,2.8,4.3,12.7,2.2,
               2.0,6.1,7.0,0.2,0.8,4.0,2.4,
               6.7,7.5,1.9,0.1, 6.0,6.3,9.1,
               2.8,1.0,2.4,0.2,2.0,0.1]

// Small helper functions
fun percent( n : int, m : int ) {
  100.0 * (n.double / m.double)
}

fun rotate( xs, n ) {
  xs.drop(n) + xs.take(n)
}

// Calculate a frequency table for a string
fun freqs( s : string ) : list<double>
{
  val lowers = list("a".code,"z".code)             // list of lower case codes
  val counts = lowers.map( fun(i) { s.count(i.string) } ) // occurrence counts
  val n      = counts.sum                          // total lower case letters
  counts.map( fun(c) { percent(c,n) } )            // occurrences in percentages
}

// Calculate how well two frequency tables match according 
// to the _chi-square_ statistic.
fun chisqr( xs : list<double>, ys : list<double> ) : double
{
  zipwith(xs,ys, fun(x,y){ ((x - y)^2.0)/y } ).sum
}

// Crack a Caesar encoded string
fun uncaesar( s : string ) : string
{
  val table  = freqs(s)                   // build a frequency table for `s`
  val chitab = list(0,25).map( fun(n) {   // build a list of chisqr numbers for each shift between 0 and 25
                  chisqr( table.rotate(n), english ) 
               })   
  val min    = chitab.minimum()           // find the mininal element
  val shift  = chitab.index-of( fun(f){ f == min } ).negate  // and use its position as our shift
  s.encode( shift )
}
  
public fun test-uncaesar() {
  println( uncaesar( "nrnd lv d ixq odqjxdjh" ) )
}

public fun main() {
  val s = "Koka is a well-typed language"
  println( "plain  : " + s)
  println( "encoded: " + s.caesar)
  println( "cracked: " + uncaesar( s.caesar ))
}