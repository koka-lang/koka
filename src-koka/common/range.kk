struct source (pub name: string, pub contents: string)

val sourceNull = Source("", "")

struct pos (pub source: source, pub off: int, pub line: int, pub col: int)

val posNull = Pos(sourceNull, -1, 0, 0)

fun (==)(p1: pos, p2: pos): bool
  p1.line == p2.line && p1.col == p2.col

fun (<)(p1: pos, p2: pos): bool
  if p1.line < p2.line then
    True
  elif p1.line > p2.line then
    False 
  else
    p1.col < p2.col

fun (<=)(p1: pos, p2: pos): bool
  if p1.line <= p2.line then
    True
  elif p1.line > p2.line then
    False 
  else
    p1.col <= p2.col

fun showPos(p: pos, alignWidth: int)
  p.line.showLine() ++ "," ++ p.col.show.pad-left(alignWidth)

fun showFullPos(p: pos, alignWidth: int)
  "[" ++ p.off.show ++ "] " ++ p.showPos(alignWidth)

// about 67 million lines
val bigLine: int = 2.pow(26)

fun showLine(line: int)
  if line > bigLine then
    "(" ++ (line - bigLine).show ++ ")"
  else if line <= 0 then
    "1"
  else
    line.show

fun posMoves8(p: pos, cs: list<char>): pos
  cs.foldl(p, posMove8)

fun posMove8(Pos(s, o, l, c), ch: char): pos
  val o1 = if o < 0 then o else o+1
  match ch
    '\t' -> Pos(s, o1, l, ((c + tabSize - 1) / tabSize)*tabSize +1)
    '\n' -> Pos(s, o1, l + 1, 1)
    _    -> Pos(s, o1, l, c + 1)

val tabSize = 2 // Always 2 in koka

// source range
struct range (pub start: pos, pub end: pos)

fun (==)(r1: range, r2: range): bool
  r1.start == r2.start && r1.end == r2.end

fun showRange(r: range, endToo: bool)
  if r.start.line > bigLine then 
    "" 
  else 
    var range := r.start.showPos(0)
    if endToo then 
      range := "(" ++ r.start.showPos(0) ++ "-" ++ r.end.showPos(0) ++ ")"     
    r.start.source.name ++ range


// Does r2 start after r1
fun after(r1: range, r2: range): bool
  r1.end <= r2.start

val rangeNull = makeRange(posNull, posNull)

fun showFullRange(r: range): string
  r.showRange(True)

fun makeRange(p1: pos, p2: pos): range
  Range(minPos(p1, p2), maxPos(p1, p2))

fun source(r: range): source
  r.start.source

fun combine(r1: range, r2: range): range
  Range(minPos(r1.start, r2.start), maxPos(r1.end, r2.end))

fun combine(rs: list<range>): range
  rs.foldl(rangeNull, combine)

fun minPos(p1: pos, p2: pos): pos
  if p1.line <= 0 then
    p2
  elif p2.line <= 0 then
    p1
  elif (p1 < p2) then
    p1
  else
    p2

fun maxPos(p1: pos, p2: pos): pos
  if (p1 < p2) then
    p2
  else
    p1

fun extend(r: range, off: int): range
  val e = r.end
  Range(r.start, e(col = e.col + off))

fun range(m: maybe<range>): range
  match m
    Just(r) -> r
    Nothing    -> rangeNull

fun sourceFromRange(Range(start, end)): string
  if start.off >= 0 then
     " ".replicate(start.col - 1).join ++ start.source.contents.slice.substr(start.off, end.off).string
  else
    val l1 = if start.line >= bigLine then 1 else start.line
    val l2 = if end.line >= bigLine then 
        if start.line >= bigLine then 
          end.line - start.line + 1 
        else 1
      else end.line
    val c1 = start.col
    val c2 = end.col
    match start.source.contents.lines.drop(l1 - 1).take(l2 - l1 + 1)
      Cons(l, ls) ->
        match (" ".replicate(c1 - 1) ++ Cons(l.list.drop(c1 - 1).string, ls)).reverse
          Cons(ll, lls) -> Cons(ll, lls).take(c2).reverse.unlines
          Nil -> ""
      Nil -> ""

fun rawSourceFromRange(Range(start, end)): string
  val s: sslice = start.source.contents.slice
  s.substr(start.off, end.off).string