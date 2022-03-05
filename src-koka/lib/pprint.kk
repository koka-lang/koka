import lib/printer

alias docs = list<doc>
type doc
  Empty
  Char(c: char) // char cannot be \n
  Text(s: string) // text does not contain \n
  Line(b: bool) // True when undone by group, do not insert a space
  Cat(d1: doc, d2: doc)
  Nest(i: int, d: doc)
  Union(d1: doc, d2: doc) // invariant: first lines of first doc longer than the first lines of the second doc
  Column(f: int -> doc)
  Nesting(f: int -> doc)
  Colored(b: bool, c: color, d: doc)
  ColoredEnd

type simpleDoc
  SEmpty
  SChar(i: int, c: char, d: simpleDoc)
  SText(i: int, s: string, d: simpleDoc)
  SLine(i: int, d: simpleDoc)
  SColorOpen(b: bool, c: color, d: simpleDoc)
  SColorClose(d: simpleDoc)

val empty = Empty

fun char(c: char): doc
  if c == '\n' then line else Char(c)

fun text(s: string): doc
  if s == "" then empty else Text(s)

val line = Line(False)
val linebreak = Line(True)
