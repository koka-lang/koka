
effect amb {
  ctl flip() : bool
}


fun amb_handle() {
  handler {
    return(x)  -> [x]
    ctl flip() -> [resume(False)]
  }
}
