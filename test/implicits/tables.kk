
effect table { fun mkTable(rows : list<list<string>>) : () }
effect tr { fun mkTr(cells : list<string>) : () }
effect td { fun mkTd(cell : string) : () }

// a generic writer effect and a handler aggregating the results in a list
effect write<r> { fun write(value: r): () }
val listWriter = handler {
  return a -> [];
  write(v) -> [v] + resume(())
}

// "renaming" / "fowarding" tr/td ~> write
val trToWrite = handler { fun mkTr(v) { write(v) } }
val tdToWrite = handler { fun mkTd(v) { write(v) } }

// mixing the different notations (with and using)
fun table(action: () -> tr ()) : table () {
  with return a -> mkTable(a)
  using listWriter
  with fun mkTr(v) { write(v) }
  action()
}
fun tr(action: () -> td ()) : tr () {
  mkTr(listWriter { tdToWrite(action) })
}
fun td(action: () -> string) : td () {
  mkTd(action())
}

fun example() {
  table {
    tr {
      td { "test1" }
      td { "test2" }
      td { "test3" }
    }
    tr {
      td { "test4" }
      td { "test5" }
      td { "test6" }
    }
  }
}
