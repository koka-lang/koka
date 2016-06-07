module caesar

extern glyph( s : string, ofs : int = 0 ) : int {
  js inline "#1.charCodeAt(#2 <= 0 ? 0 : (#2 >= (#1.length) ? 0 : #2))"
}

fun map( s : string, f : int -> e int ) : e string {
  s.map( fun(c : char) {
    f(c.int).char
  })
}

fun count( s : string, i : int ) : int {
  s.count(i.char)
}

public fun encode(s : string, shift : int = 3) {
  fun encode-glyph( c : int ) {
    val base = c - "a".glyph
    if (base < 0 || base > 25) return c
    val rot  = (base + shift) % 26
    rot + "a".glyph
  }
  s.map(encode-glyph)
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
  val lowers = list("a".glyph,"z".glyph)           // list of lower case codes
  val counts = lowers.map( fun(i) { s.count(i) } ) // occurrence counts
  val n      = counts.sum                          // total lower case letters
  counts.map( fun(c) { percent(c,n) } )            // occurrences in percentages
}

// Calculate how well two frequency tables match according 
// to the _chi-square_ statistic.
fun chisqr( xs : list<double>, ys : list<double> ) : double
{
  zipWith(xs,ys, fun(x,y){ ((x - y)^2.0)/y } ).sum
}

// Crack a Caesar encoded string
fun uncaesar( s : string ) : string
{
  val table  = freqs(s)                   // build a frequency table for `s`
  val chitab = list(0,25).map( fun(n) {   // build a list of chisqr numbers for each shift between 0 and 25
                  chisqr( table.rotate(n), english ) 
               })   
  val min    = chitab.minimum()           // find the mininal element
  val shift  = chitab.indexOf( fun(f){ f == min } ).negate  // and use its position as our shift
  s.encode( shift )
}
  
public fun test-uncaesar() {
  println( uncaesar( "nrnd lv d ixq odqjxdjh" ) )
}
