implicit val width : int

// bug: the length of function names seems to influence the order in which they
//      are being type checked. Only with length >= 4, .val-width is
//      type checked before the function using it.

// error:
//      identifier .val-width is undefined

// does not work
fun a() { width }
fun aa() { width }
fun aaa() { width }

// works
fun aaaa() { width }
