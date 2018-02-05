
effect amb {
  fun flip() : bool
}


fun amb_handle() {
  handler {
    return x -> [x]
    flip()   -> [resume(False)]
  }
}
