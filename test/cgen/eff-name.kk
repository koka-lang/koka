// https://github.com/koka-lang/koka/issues/684

pub effect test
  fun register-test(name: string): ()

pub effect test-report
  fun report-end(test: string, failures: int): ()

pub fun plain-reporter(action: () -> <test-report,io> ()): io ()
  handle<test-report>(action)
    fun report-end(test, failures: int)
      println("end")

pub fun log-names(f: () -> <test,io> ()): <test-report,io> ()
  handle<test> (f)
    fun register-test(name)
      println("Test name: " ++ name)

pub fun main()
  with plain-reporter
  println("1")
  log-names
    println("2")
    register-test("Assertion")