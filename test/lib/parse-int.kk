// exponents must be < 10^9; larger ones used to be accepted and crashed on allocation
fun main()
  "1e3".parse-int.println
  "12e2".parse-int.println
  "1.5e1".parse-int.println
  "1e30".parse-int.println
  "1e999999999999999999".parse-int.println
  "1e1000000000".parse-int.println
