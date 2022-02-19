// test for TAOCP Vol 2 bigint division bug (1st & 2nd edition), 
// see: <https://blog.adacore.com/proving-the-correctness-of-gnat-light-runtime-library>

noinline fun muldiv( x : int, y : int, z : int ) : int 
  (x*y)/z

pub fun main()
  muldiv(18446744069414584318, 4294967296, 18446744069414584319).println // 4294967295
