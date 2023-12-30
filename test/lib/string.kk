// https://github.com/koka-lang/koka/issues/287
fun main()
  "".to-upper.println
  "abc".to-upper.println
  "".to-lower.println
  "ABC".to-lower.println
  "".is-empty.println
  " ".is-empty.println
  "".is-notempty.println
  " ".is-notempty.println