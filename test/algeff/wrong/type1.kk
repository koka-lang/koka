
effect amb {
  flip() : bool
}


function amb_handle() {
  handler {
    return x -> [x]
    flip()   -> [resume(False)]
  }
}
