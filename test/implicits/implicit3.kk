import std/text/regex

// ambient Value Bindings
ambient val cwd : string

fun home?() {
  // println(cwd)
  cwd.starts-with?("/home/")
}

fun cd(path,action) {
  with val cwd = cwd + path + "/"
  action()
}

fun cd2a(path,action) {
  with val cwd = cwd + path + "/"
  mask behind<cwd>(action)
}

fun cd2b(path,action) {
  val cur = cwd
  with mask<cwd>
  with val cwd = cur + path + "/"
  action()
}

fun cd2c(path,action) {
  with val cwd = cwd + path + "/"
  action()
}

fun ex1() {
  with val cwd = "/"
  println(home?())
  println(cd("home") { home?() })
  println(cd("var") { cd("lib") { home?() } })
}

fun ex2() {
  with val cwd = { print("hello "); "/" }
  print("world")
  cwd + cwd
}


// ambient Function Bindings
ambient fun log(msg : string) : ()

fun log-working-dir() : <cwd, log> () {
  log("current working dir is: " + cwd)
}

fun ex3() {
  with val cwd = "/"
  with fun log(msg) { append-to-file(cwd + "log.txt", msg) }
  cd("var") { cd("lib") { log-working-dir() } }
}

// ambient Control Bindings
ambient control read-file(filename : string) : string

fun cat(name) {
  read-file(cwd + name)
}

fun ex4() {
  with val cwd = "/var/lib"
  with control read-file(f) { read-async(f, resume) }
  cd("var") {
    cd("lib") {
      println(cat("file.txt"))
    }
    println("exit lib")
  }
  println("exit var")
}

fun ex5() {
  with val cwd = "/var/lib"
  with control read-file(f) { resume("dummy contents for " + f) }
  cd("var") {
    cd("lib") {
      println(cat("file.txt"))
    }
    println("exit lib")
  }
  println("exit var")
}


// assumed library functions

fun starts-with?(s, pre) {
  s.starts-with("/home/").is-defined?
}

fun is-defined?(m) {
  match(m) {
    Just(_) -> True
    Nothing() -> False
  }
}

fun append-to-file(path, content) {
  println("appending to: " + path + "; content: \"" + content + "\"")
}

fun read-async(filename : string, k : string -> <console|e> ()) : <console|e> () {
  println("reading file :" + filename)
  k("contents")
  println("done reading file :" + filename)
}
