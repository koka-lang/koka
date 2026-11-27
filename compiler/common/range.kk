//----------------------------------------------------------------------------
// Copyright 2024, Microsoft Research, Daan Leijen. Tim Whiting.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//----------------------------------------------------------------------------
// Updated as of 7/23/26: Commit d026468 -- missing utfDecode, readInput (n/a: Haskell ByteString fallback; we use read-text-file), extractLiterate
module compiler/common/range

import std/os/path
import pretty/pprint

pub value struct source 
  name: string
  contents: string

pub fun source/(==)(s1: source, s2: source): bool
  s1.name == s2.name

pub val source/null = Source("", "")

// The code of a literate document (`.kk.md`) for the lexer, as upstream
// `extractLiterate`: every line keeps its place, so ranges point into the
// document. The lines of a ``` or ```koka block are code (less the fence's
// indentation), and the line of a block's closing fence is empty. Every other
// line is commented out; a block with any other info string is skipped whole. A
// `////` line in a block empties itself and the lines above it in the block
// (shown in the document, not compiled). A block left open at the end is dropped.
pub fun extract-literate(input: string): div string
  // the lines as upstream's `BC.span (/='\n')` scan visits them
  val lines0 = input.split("\n")
  val lines = match lines0.last
    Just("") -> lines0.init
    _ -> lines0
  literate-scan(False, lines).join

// a line's indentation, its count of leading backticks, and the rest
fun literate-fence(line: string): (int, int, string)
  val indent = line.list.take-while(fn(c) c == ' ').length
  val iline = line.list.drop(indent)
  val qs = iline.take-while(fn(c) c == '`').length
  (indent, qs, iline.drop(qs).string)

fun literate-white(cs: string): bool
  cs.list.all(fn(c) c == ' ' || c == '\r' || c == '\n' || c.int == 11 || c.int == 12)  // and \v \f

fun literate-scan(skipping: bool, ls: list<string>): div list<string>
  match ls
    Nil -> Nil
    Cons(line, rest) ->
      val (indent, qs, cs) = literate-fence(line)
      if qs == 3 && !skipping && (cs.literate-white || cs.starts-with("koka").bool)
        then Cons("\n", literate-scan-code(indent, [], rest))
        else Cons("//" ++ line ++ "\n", literate-scan(if qs == 3 then !skipping else skipping, rest))

fun literate-scan-code(indent: int, acc: list<string>, ls: list<string>): div list<string>
  match ls
    Nil -> Nil
    Cons(line, rest) ->
      val (_, qs, cs) = literate-fence(line)
      if qs == 3 && cs.literate-white
        then Cons("", acc).reverse.map(fn(l) l ++ "\n") ++ literate-scan(False, rest)
      elif cs.starts-with("////").bool
        then literate-scan-code(indent, Cons("", acc.map(fn(_) "")), rest)
      else literate-scan-code(indent, Cons(line.list.drop(indent).string, acc), rest)

pub struct pos
  source: source
  off: int
  line: int
  col: int

pub val pos/null = Pos(source/null, -1, 0, 0)

pub fun pos/(==)(p1: pos, p2: pos): bool
  p1.line == p2.line && p1.col == p2.col

pub fun pos/(<)(p1: pos, p2: pos): bool
  if p1.line < p2.line then
    True
  elif p1.line > p2.line then
    False 
  else
    p1.col < p2.col

pub fun pos/(<=)(p1: pos, p2: pos): bool
  if p1.line < p2.line then
    True
  elif p1.line > p2.line then
    False 
  else
    p1.col <= p2.col

pub fun pos/(>)(p1: pos, p2: pos): bool
  if p1.line > p2.line then
    True
  elif p1.line < p2.line then
    False 
  else
    p1.col > p2.col

pub fun pos/(>=)(p1: pos, p2: pos): bool
  if p1.line > p2.line then
    True
  elif p1.line < p2.line then
    False 
  else
    p1.col >= p2.col

pub fun pos/cmp(p1: pos, p2: pos): order
  match p1.line.cmp(p2.line)
    Eq -> p1.col.cmp(p2.col)
    o -> o

pub fun pos/show(p: pos): string
  "(" ++ p.show-pos(?align-width=2) ++ ")"

pub fun show-pos(p: pos, ?align-width: int): string
  p.line.show-line() ++ "," ++ p.col.show.pad-left(align-width)

pub fun show-full-pos(p: pos, ?align-width: int): string
  "[" ++ p.off.show ++ "] " ++ p.show-pos()

// about 67 million lines
val big-line: int = 2.pow(26)

// Lines from `big-line` on belong to the interpreter's input and show relative to it.
pub fun show-line(line: int)
  if line >= big-line then
    "(" ++ (line - big-line).show ++ ")"
  else if line <= 0 then
    "1"
  else
    line.show

// Offsets and columns count utf-8 BYTES, as the lexer does: a multi-byte character advances both by its encoded length.
pub fun chars/pos-moves8(p: pos, cs: list<char>): pos
  cs.foldl(p, pos-move8)

pub fun sslice/pos-moves8(p: pos, cs: sslice): pos
  cs.string.list.foldl(p, pos-move8)

pub fun pos-move8(Pos(s, o, l, c), ch: char): pos
  val n = ch.utf8-length
  val o1 = if o < 0 then o else o + n
  match ch
    '\t' -> Pos(s, o1, l, ((c + tab-size - 1) / tab-size)*tab-size +1)
    '\n' -> Pos(s, o1, l + 1, 1)
    _    -> Pos(s, o1, l, c + n)

fun utf8-length(ch: char): int
  val i = ch.int
  if i < 0x80 then 1 elif i < 0x800 then 2 elif i < 0x10000 then 3 else 4

val tab-size = 2 // Always 2 in koka

// source range
pub struct range
  start: pos
  end: pos
  hidden: bool

pub fun range/(==)(r1: range, r2: range): bool
  r1.start == r2.start && r1.end == r2.end && r1.hidden == r2.hidden

pub fun show-compact-range(r: range)
  "[" ++ r.start.show-pos(0) ++ "," ++ r.end.show-pos(0) ++ "]"

// A diagnostic's path relative to the compile's working directory when it
// sits underneath it, so a message reads `samples/learn/lazycons.kk` instead
// of an absolute path. Components are compared WHOLE, so `/foo/ba` is not a
// prefix of `/foo/bar/x.kk`.
//
// Upstream's `showRange` calls `relativeToPath` (Common/File.hs). This is a
// string version because converting the source name to a `path` would
// normalize it (resolving `..`) and change the name the diagnostic shows.
fun relative-to-cwd(src: string, cwd: string): string
  fun parts(s: string): list<string>
    s.list.map(fn(c) if c == '/' || c == '\\' then '/' else c).string
     .split("/").filter(fn(x) !x.is-empty)
  // length + zip rather than recursion, so this stays total (a recursive
  // helper here would make every `show-range` -- and so every diagnostic -- div)
  fun prefix-of(xs: list<string>, ys: list<string>): bool
    xs.length <= ys.length && zip(xs, ys).all(fn((x, y)) x == y)
  if cwd.is-empty then src
  else
    val cs = cwd.parts
    val ss = src.parts
    if !cs.is-nil && prefix-of(cs, ss) then ss.drop(cs.length).join("/")
    else src

pub fun show-range(r: range, cwd: path, endToo: bool)
  val path = 
        if r.start.line >= big-line then ""
        else r.start.source.name.relative-to-cwd(cwd.string)
  if endToo then 
    path ++ "(" ++ r.start.show-pos(0) ++ "-" ++ r.end.show-pos(0) ++ ")"
  else path ++ r.start.show

pub fun range/cmp(r1: range, r2: range): order
  match r1.start.cmp(r2.start)
    Eq -> r1.end.cmp(r2.end)
    o -> o

pub fun range/pretty(r: range): doc
  r.show-compact-range.pretty

// Does r2 start after r1
pub fun range/is-after(r1: range, r2: range): bool
  r1.end <= r2.start

pub val range/null = Range(pos/null, pos/null, False)

pub fun range/is-null(r: range)
  r.start.off < 0 || r.end.off < 0

// The start position only; `show-full-range` includes the source and the end.
pub fun range/show(r: range): string
  r.start.show

pub fun show-full-range(r: range, cwd: path): string
  r.show-range(cwd, True)

pub fun make-range(p1: pos, p2: pos): _ range
  if p1.source != p2.source then throw("range/make-range: positions are from different sources")
  Range(min(p1, p2), max(p1, p2), False)

pub fun make-source-range(p: path, l1: int, c1: int, l2: int, c2: int)
  val src = Source(p.string, "")
  make-range(Pos(src, -1, l1, c1), Pos(src, -1, l2, c2))

pub fun range/length(r: range): int
  r.end.off - r.start.off

pub fun source(r: range): source
  r.start.source

pub fun combine(r1: range, r2: range): range
  Range(min(r1.start, r2.start), max(r1.end, r2.end), r1.hidden || r2.hidden)

pub fun list/range(l: list<a>, ?range: a -> range): range
  range-list/combine(l.map(range))

pub fun range-list/combine(rs: list<range>): range
  rs.foldl(range/null, combine)

pub fun range/hide(r: range)
  Range(r.start, r.end, True)

// upstream minPos/maxPos compare with `<=`: on a TIE (equal line/col --
// which every single-character token has, since ranges end inclusively) the
// ORIGINAL argument order is preserved. `<` would swap the two positions on
// ties, exchanging their offsets (start.off > end.off => empty source slices).
pub fun pos/min(p1: pos, p2: pos): pos
  if p1.line <= 0 then // for combining nullRanges sensibly
    p2
  elif p2.line <= 0 then
    p1
  elif (p1 <= p2) then
    p1
  else
    p2

pub fun pos/max(p1: pos, p2: pos): pos
  if (p1 <= p2) then
    p2
  else
    p1

pub fun extend(r: range, off: int): range
  val e = r.end
  Range(r.start, e(col = e.col + off), r.hidden)

// Create a range for the first character in the range
pub fun start-of-range(r: range): range
  Range(r.start, r.start, r.hidden)

// Create a range for the final character in the range
pub fun end-of-range(r: range): range
  if r.end.off - r.start.off <= 1 then r
  else Range(r.end, r.end, r.hidden)

pub fun range/contains(r: range, p: pos): bool
  r.start <= p && r.end >= p

pub fun range/is-before(r: range, p: pos): bool
  r.end < p

pub fun range/starts-at(r: range, p: pos): bool
  r.start == p

pub fun range/just-before(r: range): range
  if r.start.off < 0 then r
  else
    match r.start
      Pos(src, off, l, c) -> 
        val newst = Pos(src, off - 1, l, c - 1)
        Range(newst, newst, r.hidden)

pub fun range/just-after(r: range): range
  if r.end.off < 0 then r
  else
    match r.end
      Pos(src, off, l, c) -> 
        val newst = Pos(src, off + 1, l, c + 1)
        Range(newst, newst, r.hidden)

// From Layout.hs
pub fun range/after(r: range): range
  Range(r.end, r.end, r.hidden)

// From Layout.hs
pub fun range/before(r: range): range
  Range(r.start, r.start, r.hidden)

pub fun range/default(m: maybe<range>): range
  match m
    Just(r) -> r
    Nothing -> null

pub fun source-from-range(Range(start, end)): string
  if start.off >= 0 then
     // positions carry BYTE offsets (the lexer works on utf-8 bytes), so the
     // slice must be BYTE-based -- sslice/subslice counts CHARACTERS and drifts after any
     // multi-byte character (test syntax/wrong/utf1)
     " ".replicate(start.col - 1).join ++ start.source.contents.bytes.substring(start.off, end.off - start.off)
  else
    val l1 = if start.line >= big-line then 1 else start.line
    val l2 = if end.line >= big-line then 
        if start.line >= big-line then 
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

pub fun raw-source-from-range(Range(start, end)): string
  start.source.contents.bytes.substring(start.off, end.off - start.off)