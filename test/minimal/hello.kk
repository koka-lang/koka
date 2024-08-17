extern doprint(s : string) : ()
  vm "println(String): Unit"

pub fun main()
  doprint("Hello")
