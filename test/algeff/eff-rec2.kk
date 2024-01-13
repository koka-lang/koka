// https://github.com/koka-lang/koka/issues/385
pub rec effect parse
  fun current-input() : sslice

pub fun parse-safe( input0 : sslice, p)
  var input := input0
  with handler
    fun current-input()  
      input 
  p()

pub fun main()
  with parse-safe("hello".slice)
  val x = current-input()
  x.show.println