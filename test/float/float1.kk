effect foo 
  fun boo(x : int) : int


fun main() 
  with fun boo(x){ x }
  boo(42)
