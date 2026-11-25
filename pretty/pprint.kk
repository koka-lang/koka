/*----------------------------------------------------------------------------
   Copyright 2024-2026, Tim Whiting.

   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0. A copy of the License can be
   found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
// Translated from koka/src/Lib/PPrint.hs, but using different printers
// Updated 2/10/25 Commit d7b02a2
// Pretty printing via the effect handlers doesn't allow for as much control
// of indentation and alignment as the original implementation.
// We use effects on the printer layer though, which allows for the backend to be configured separately

// Pretty printing (Wadler/Leijen).
// Not part of the standard library: its API follows upstream's `Lib/PPrint.hs` because the whole compiler prints through it.
module pretty/pprint
import pretty/printer
import std/core/undiv
import std/os/path
import std/os/file
import std/num/float64

pub infixr 5 (/), (||), (|-|), (|--|) 
pub infixr 6 (|.|), (|+|)

// List, tupled and semi-braces pretty print a list of documents
// either horizontally or vertically aligned.

pub fun list(l: list<doc>): doc
  enclose-sep(l, lbracket, rbracket, comma)

pub fun tupled(l: list<doc>): doc
  l.enclose-sep(lparen, rparen, text(", "))

pub fun semi-braces(l: list<doc>): doc
  l.enclose-sep(lbrace, rbrace, semi)

pub fun angled(l: list<doc>): doc
  l.enclose-sep(langle, rangle, comma)

pub fun enclose-sep(lst: list<doc>, l: doc, r: doc, p: doc): doc
  match lst
    Nil -> l |.| r
    Cons(c, Nil) -> l |.| c |.| r
    _ -> 
      val x = l |.| lst.punctuate(p).hcat |.| r
      x.nest(2) 

pub fun punctuate(l: list<doc>, p: doc): list<doc>
  match l
    Nil -> Nil
    Cons(c, Nil) -> Cons(c, Nil)
    Cons(c, cs) -> Cons(c |.| p, punctuate(cs, p)) 

// High level combinators
pub fun sep(l: list<doc>): doc
  l.vsep.group

pub fun fill-sep(l: list<doc>): doc
  l.fold((/))

pub fun hsep(l: list<doc>): doc
  l.fold((|+|))

pub fun vsep(l: list<doc>): doc
  l.filter(!_.is-empty).fold((|--|))

pub fun cat(l: list<doc>): doc
  l.vcat.group

pub fun fill-cat(l: list<doc>): doc
  l.fold((||))

pub fun hcat(l: list<doc>): doc
  l.fold((|.|))

pub fun vcat(l: list<doc>): doc
  l.filter(!_.is-empty).fold((|--|))

pub fun fold(l: list<doc>, f: (doc, doc) -> doc): doc
  match l
    Nil -> empty
    Cons(x, xs) -> xs.foldl(x, f)

pub fun (|.|)(x: doc, y: doc): doc
  x.beside(y)

pub fun (|+|)(x: doc, y: doc): doc
  x |.| space |.| y

pub fun (/)(x: doc, y: doc): doc
  if x.is-empty then y else if y.is-empty then x else x |.| softline |.| y

pub fun (||)(x: doc, y: doc): doc
  x |.| softbreak |.| y

pub fun (|-|)(x: doc, y: doc): doc
  if x.is-empty then y else if y.is-empty then x else x |.| line |.| y

pub fun (|--|)(x: doc, y: doc): doc
  x |.| linebreak |.| y

pub val softline = line.group
pub val softbreak = linebreak.group

pub fun squotes(d: doc): doc
  d.enclose(squote, squote)

pub fun dquotes(d: doc): doc
  d.enclose(dquote, dquote)

pub fun braces(d: doc): doc
  d.enclose(lbrace, rbrace)

pub fun parens(d: doc): doc
  d.enclose(lparen, rparen)

pub fun angles(d: doc): doc
  d.enclose(langle, rangle)

pub fun brackets(d: doc): doc
  d.enclose(lbracket, rbracket)

pub fun enclose(x: doc, l: doc, r: doc): doc
  l |.| x |.| r

pub val lparen = Char('(')
pub val rparen = Char(')')
pub val langle = Char('<')
pub val rangle = Char('>')
pub val lbrace = Char('{')
pub val rbrace = Char('}')
pub val lbracket = Char('[')
pub val rbracket = Char(']')

pub val squote = Char('\'')
pub val dquote = Char('"')
pub val semi = Char(';')
pub val colon = Char(':')
pub val comma = Char(',')
pub val space = Char(' ')
pub val dot = Char('.')
pub val backslash = Char('\\')
pub val equals = Char('=')

// string is like "text" but replaces '\n' by "line"
pub fun string/pretty(s: string): doc
  if s == "" then empty else
    match s.starts-with("\n")
      Just(x) -> line |.| pretty(x.string.pretend-decreasing)
      _ -> 
        val (xs, ys) = span(s.list, _ != '\n')
        text(xs.string) |.| pretty(ys.string.pretend-decreasing)

pub fun bool/pretty(b: bool): doc
  b.show.text

pub fun int/pretty(i: int): doc
  i.show.text

pub fun float64/pretty(d: float64): doc
  d.show.text

pub fun unit/pretty(unit: ()): doc
  "()".text

pub fun show-pretty(a: a, ?show: a -> e string) : e doc
  a.show.text

pub fun list/pretty(a: list<a>, ?pretty: a -> e doc): e doc
  a.map(pretty).list

pub fun tuple/pretty((a,b): (a, b), ?a/pretty: a -> e doc, ?b/pretty: b -> e doc): e doc
  tupled([a.pretty, b.pretty])

pub fun triple/pretty((a,b,c): (a, b, c), ?a/pretty: a -> e doc, ?b/pretty: b -> e doc, ?c/pretty: c -> e doc): e doc
  tupled([a.pretty, b.pretty, c.pretty])

pub fun maybe/pretty(a: maybe<a>, prefix-space: bool = False, ?pretty: a -> e doc): e doc
  match a
    Just(x) -> if prefix-space then empty |+| x.pretty else x.pretty
    Nothing -> empty

// Semi primitive: fill and fill-break

pub fun fill-break(d: doc, f: int): doc
  d.width(fn(w) if w > f then linebreak.nest(f) else spaces(f - w).text)

pub fun fill(d: doc, f: int): doc
  d.width(fn(w) if w >= f then empty else spaces(f - w).text)

pub fun width(d: doc, f: (int -> doc)): doc
  column(fn(k1) d |.| column(fn(k2) f(k2 - k1)))

// Semi primitive: Alignment and indentation

pub fun indent(d: doc, i: int): doc
  (spaces(i).text |.| d).hang(i)

pub fun hang(d: doc, i: int): doc
  d.nest(i).align()

pub fun align(d: doc): doc
  column(fn(k1:int) nesting(fn(i1) d.nest(k1 - i1)))

// primitives

pub alias docs = list<doc>
pub type doc
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

pub type simple-doc
  SEmpty
  SChar(i: int, c: char, d: simple-doc)
  SText(i: int, s: string, d: simple-doc)
  SLine(i: int, d: simple-doc)
  SColorOpen(b: bool, c: color, d: simple-doc)
  SColorClose(d: simple-doc)

pub fun make-markdown(d: doc): doc
  match d
    Line(True) -> "\n\n".text
    Column(f) -> Column(fn(x) make-markdown(f(x)))
    Nesting(f) -> Nesting(fn(x) make-markdown(f(x)))
    Cat(x, y) -> Cat(make-markdown(x), make-markdown(y))
    Nest(i, x) -> Nest(i, make-markdown(x))
    Union(x, y) -> Union(make-markdown(x), make-markdown(y))
    Colored(b, c, x) -> Colored(b, c, make-markdown(x))
    _ -> d

pub val empty = Empty

pub fun char(c: char): doc
  if c == '\n' then line else Char(c)

pub fun text(s: string): doc
  if s == "" then empty else Text(s)

pub val line = Line(False)
pub val linebreak = Line(True)

pub inline fun beside(x: doc, y: doc): doc
  if x.is-empty then y else if y.is-empty then x else Cat(x, y)

pub inline fun nest(x: doc, i: int): doc
  Nest(i, x)

pub inline fun column(f: int -> doc): doc
  Column(f)

pub inline fun nesting(f: int -> doc): doc
  Nesting(f)

pub inline fun group(d: doc): doc
  Union(d.flatten, d)

pub inline fun color(d: doc, c: color): doc
  Colored(True, c, d)

pub inline fun bcolor(d: doc, c: color): doc
  Colored(False, c, d)

pub fun flatten(d: doc): doc
  match d
    Cat(x, y) -> Cat(flatten(x), flatten(y))
    Nest(i, x) -> Nest(i, flatten(x))
    Line(b) -> if b then Empty else Text(" ")
    Union(x, _) -> flatten(x)
    Column(f) -> Column(fn(x) flatten(f(x)))
    Nesting(f) -> Nesting(fn(x) flatten(f(x)))
    Colored(b, c, x) -> Colored(b, c, flatten(x))
    x -> x // Empty,Char,Text

pub fun doc-contains(d: doc, p: (string -> e bool)): e bool
  d.texts.any(p)

pub fun doc-starts-with(d: doc, s: string): bool
  doc-starts-withT(d.texts, s, s.count)

pub fun doc-starts-withT(l: list<string>, pre: string, n: int)
  match l
    Nil -> n == 0
    Cons(s, xs) ->
      val m = s.count
      if m >= n then
        s.slice.take(n).string == pre
      else
        pre.slice.take(m).string == s && doc-starts-withT(xs, pre.slice.subslice(m,pre.count).string, n - m)

// TODO: Optimize a bunch of this
pub fun doc-ends-with(d: doc, s: string): bool
  doc-ends-withT(d.rtexts, s.list.reverse.string, s.count)

pub fun doc-ends-withT(l:list<string>, rpost: string, n:int)
  match l
    Nil -> n == 0
    Cons(s, xs) ->
      val m = s.count
      if m >= n then
        s.slice.take(n).string == rpost
      else
        rpost.slice.take(m).string == s && doc-ends-withT(xs, rpost.slice.subslice(m,rpost.count).string, n - m)

pub fun texts(d: doc): list<string>
  match d
    Empty -> []
    Char(c) -> [c.string]
    Text(s) -> [s]
    Line(br) -> if br then [] else ["\n"]
    Union(x, _) -> x.texts
    Cat(x, y) -> x.texts ++ y.texts
    Nest(_, x) -> x.texts
    Column(f) -> f(0).texts
    Nesting(f) -> f(0).texts
    Colored(_, _, x) -> x.texts
    ColoredEnd -> []

pub fun rtexts(d: doc): list<string> // reversed text fragments
  match d
    Empty -> []
    Char(c) -> [c.string]
    Text(s) -> [s.list.reverse.string]
    Line(br) -> if br then [] else ["\n"]
    Union(x, _) -> x.rtexts
    Cat(x, y) -> x.rtexts ++ y.rtexts
    Nest(_, x) -> x.rtexts
    Column(f) -> f(0).rtexts
    Nesting(f) -> f(0).rtexts
    Colored(_, _, x) -> x.rtexts
    ColoredEnd -> []

// renderers
pub fun doc/(==)(d1: doc, d2: doc): bool
  d1.render-compact.display-string == d2.render-compact.display-string

// render-pretty: the default pretty printing algorithm

// list of indentation/document pairs; saves an indirection over list<(int, doc)>
type doc-list
  DocNil
  DocCons(i: int, d: doc, dl: doc-list)

pub fun render-pretty-b(rfrac: float64, w: int, xdoc: doc): div string
  // r - the ribbon width in characters
  val r = max(0, min(w, (w.float64 * rfrac).int))

  // r = ribbon with, w = page width, n = indentation of current line, k = current column
  // x and y, the (simple) documents to chose from.
  // precondition: first lines of x are longer than the first lines of y
  fun nicest(n, k, x, y)
    x // TODO: reimplement this according to render-pretty

  // b = base nesting, n = indentation of current line, k = current column
  // (k >= n) && (k - n == count of inserted characters)
  fun best(b, n, k, l)
    match l
      Nil -> ""
      Cons((i, d), ds) ->
        match d
          Empty -> best(b, n, k, ds)
          Char(c) -> c.string ++ best(b, n, k + 1, ds)
          Text(s) -> s ++ best(b, n, k + s.count, ds)
          Line(_) -> "\n" ++ best(b, i, i, ds)
          Cat(x, y) -> best(b, n, k, Cons((i, x), Cons((i, y), ds)))
          Nest(j, x) ->
            val i' = i + j
            best(if b == 0 then i' else b, n, k, Cons((i', x), ds))
          Union(x, y) -> nicest(n, k, best(b, n, k, Cons((i, x), ds)), best(b, n, k, Cons((i, y), ds)))
          Column(f) -> best(b, n, k, Cons((i, f(k)), ds))
          Nesting(f) -> best(b, n, k, Cons((i, f(i)), ds))
          Colored(_, _, x) -> best(b, n, k, Cons((i, x), ds))
          ColoredEnd -> best(b, n, k, ds)
  best(0, 0, 0, [(0, xdoc)])

pub fun render-pretty(rfrac: float64, w: int, xdoc: doc): div simple-doc
  // r - the ribbon width in characters
  val r = max(0, min(w, (w.float64 * rfrac).int))

  // r = ribbon with, w = page width, n = indentation of current line, k = current column
  // x and y, the (simple) documents to chose from.
  // precondition: first lines of x are longer than the first lines of y
  fun nicest(n, k, x, y)
    val width = min(w - k, r - k + n)
    if fits(width, x) then x else y

  // b = base nesting, n = indentation of current line, k = current column
  // (k >= n) && (k - n == count of inserted characters)
  fun best(b, n, k, l)
    match l
      Nil -> SEmpty
      Cons((i, d), ds) ->
        match d
          Empty -> best(b, n, k, ds)
          Char(c) -> SChar(b, c, best(b, n, k + 1, ds))
          Text(s) -> SText(b, s, best(b, n, k + s.count, ds))
          Line(_) -> SLine(i, best(b, i, i, ds))
          Cat(x, y) -> best(b, n, k, Cons((i, x), Cons((i, y), ds)))
          Nest(j, x) ->
            val i' = i + j
            best(if b == 0 then i' else b, n, k, Cons((i', x), ds))
          Union(x, y) -> nicest(n, k, best(b, n, k, Cons((i, x), ds)), best(b, n, k, Cons((i, y), ds)))
          Column(f) -> best(b, n, k, Cons((i, f(k)), ds))
          Nesting(f) -> best(b, n, k, Cons((i, f(i)), ds))
          Colored(f, c, x) -> SColorOpen(f, c, best(b, n, k, Cons((i, x), Cons((i, ColoredEnd), ds))))
          ColoredEnd -> SColorClose(best(b, n, k, ds))
  best(0, 0, 0, [(0, xdoc)])

pub fun fits(w: int, x: simple-doc)
  if w < 0 then False
  else match x
    SEmpty -> True
    SChar(_, _, x) -> fits(w - 1, x)
    SText(_, s, x) -> fits(w - s.count, x)
    SLine(_, _) -> True
    SColorOpen(_, _, x) -> fits(w, x)
    SColorClose(x) -> fits(w, x)

// render-compact: renders documents without indentation
// fast and fewer characters output, good for machines
pub fun render-compact(xdoc: doc): simple-doc
  fun scan(k, l)
    match l
      Nil -> SEmpty
      Cons(d, ds) ->
        match d
          Empty -> scan(k, ds)
          Char(c) -> SChar(0, c, scan(k + 1, ds.pretend-decreasing))
          Text(s) -> SText(0, s, scan(k + s.count, ds.pretend-decreasing))
          Line(_) -> SLine(0, scan(0, ds.pretend-decreasing))
          Cat(x, y) -> scan(k, Cons(x, Cons(y, ds)).pretend-decreasing)
          Nest(_, x) -> scan(k, Cons(x, ds).pretend-decreasing)
          Union(_, y) -> scan(k, Cons(y, ds).pretend-decreasing)
          Column(f) -> scan(k, Cons(f(k), ds).pretend-decreasing)
          Nesting(f) -> scan(k, Cons(f(0), ds).pretend-decreasing)
          Colored(f, c, x) -> SColorOpen(f, c, scan(k, Cons(x, Cons(ColoredEnd, ds)).pretend-decreasing))
          ColoredEnd -> SColorClose(scan(k, ds.pretend-decreasing))
  scan(0, [xdoc])

pub fun as-string(d: doc)
  display-string(render-compact(d))

// Displayers: displayS and displayIO

pub fun display-string(simple: simple-doc): string
  // accumulator style: the naive right-recursive `s ++ x.display-string`
  // overflows the C stack on very large documents (e.g. a compiler emitting
  // a large module interface as one doc)
  fun go(s: simple-doc, acc: list<string>): list<string>
    match s
      SEmpty -> acc
      SChar(_, c, x) -> go(x, Cons(c.string, acc))
      SText(_, t, x) -> go(x, Cons(t, acc))
      SLine(i, x) -> go(x, Cons("\n" ++ spaces(i), acc))
      SColorOpen(_, _, x) -> go(x, acc)
      SColorClose(x) -> go(x, acc)
  go(simple, []).reverse.join

pub fun skip-spaces(simple: simple-doc): simple-doc
  fun is-space(c : char) : bool
    c == ' ' || c == '\t'
  match simple
    SChar(_, c, x) | c.is-space -> skip-spaces(x)
    SText(i, s, x) ->
      val noSpaces = s.list.drop-while(is-space).string
      if noSpaces == "" then
        skip-spaces(x)
      else
        SText(i, noSpaces, x)
    _ -> simple 

pub fun display-pretty(w: int, simple: simple-doc): <colored-printer,printer,div> ()
  fun display(curCol, simp)
    match simp
      SEmpty -> ()
      SChar(i, c, x) ->
        if curCol + 1 >= w  && i + 1 < w then
          display(curCol, SLine(i + 1, simp.skip-spaces))
        else
          write(c.string)
          display(curCol + 1, x)
      SText(i, s, x) ->
        if curCol + s.count >= w && i + s.count < w then
          display(curCol, SLine(i + 1, simp.skip-spaces))
        else
          write(s)
          display(curCol + s.count, x)
      SLine(i, x) ->
        write-ln("")
        write(spaces(i))
        display(i, x)
      SColorOpen(f, c, x) ->
        if f then 
          update-push-color-settings(_(fcolor=c))
          display(curCol, x)
        else
          update-push-color-settings(_(bcolor=c))
          display(curCol, x)
      SColorClose(x) ->
        pop-color-settings()
        display(curCol, x)
  display(0, simple)

pub fun write-pretty(d: doc)
  display-pretty(default-width, render-pretty(0.8, default-width, d))

pub fun write-pretty-ln(d: doc)
  write-pretty(d |.| linebreak)

pub fun show(d: doc): div string
  display-string(render-pretty(0.5, default-width, d))

pub fun put-doc(d: doc)
  println(render-pretty-b(0.5, default-width, d))

pub fun write-doc(fpath: path, doc: doc, width: int=default-width)
  fpath.write-text-file(render-pretty-b(0.5, width, doc))

// insert spaces "indentation" used to insert tabs, but tabs seem to cause more trouble than they solve :-)
pub fun spaces(n: int): string
  match n
    0 -> ""
    1 -> " "
    2 -> "  "
    3 -> "   "
    4 -> "    "
    5 -> "     "
    6 -> "      "
    7 -> "       "
    8 -> "        "
    9 -> "         "
    10 -> "          "
    11 -> "           "
    12 -> "            "
    13 -> "             "
    14 -> "              "
    15 -> "               "
    16 -> "                "
    17 -> "                 "
    _ -> if n <= 0 then "" else replicate(" ", n).join

val default-width = 512
