/*----------------------------------------------------------------------------
   Copyright 2024-2026, Tim Whiting.

   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0. A copy of the License can be
   found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
// Translated from koka/src/Lib/Printer.hs, but using effects all over
// Updated 7/23/24 Commit 32ada87

// Module for portable control of colors in a console
// only the color of 'stdout' is influenced by these functions, (except for the string / file variants of printers)

module pretty/printer

import std/os/path
import std/os/file
import std/os/env

pub fun is-windows()
  get-os-name() == "windows"

//--------------------------------------------------------------------------
//  Printer
//--------------------------------------------------------------------------
// A printer is an abstraction for something where we can send
// character output to.
pub linear effect printer
  fun write(text: string): ()
  fun write-ln(text: string): ()
  fun flush(): ()

// A colored printer has additional coloring and formatting options
pub linear effect colored-printer
  fun push-color-settings(s: print-color-settings): ()
  fun pop-color-settings(): ()
  fun update-push-color-settings(f: print-color-settings -> print-color-settings): ()

pub struct print-color-settings
  fcolor: color = ColorDefault
  bcolor: color = ColorDefault
  invert: bool = False
  underline: bool = False

pub fun color-settings(s: print-color-settings, action)
  push-color-settings(s)
  action()
  pop-color-settings()

pub fun fcolor(c: color, action)
  update-push-color-settings(fn(old) old(fcolor=c))
  action()
  pop-color-settings()

pub fun bcolor(c: color, action)
  update-push-color-settings(fn(old) old(bcolor=c))
  action()
  pop-color-settings()

pub fun underline(u: bool, action)
  update-push-color-settings(fn(old) old(underline=u))
  action()
  pop-color-settings()

pub fun invert(i: bool, action)
  update-push-color-settings(fn(old) old(invert=i))
  action()
  pop-color-settings()

pub fun print-colored(s: string, c: color)
  with fcolor(c)
  s.write()

pub fun println-colored(s: string, c: color)
  with fcolor(c)
  s.write-ln()

//--------------------------------------------------------------------------
//  Interface
//--------------------------------------------------------------------------
// Available colors on a console. Normally, background colors are
// converted to their /dark/ variant.
pub type color
  Black
  DarkRed
  DarkGreen
  DarkYellow
  DarkBlue
  DarkMagenta
  DarkCyan
  Gray
  DarkGray
  Red
  Green
  Yellow
  Blue
  Magenta
  Cyan
  White
  ColorDefault

fun color/show(c: color): string
  match c
    Black -> "Black"
    DarkRed -> "DarkRed"
    DarkGreen -> "DarkGreen"
    DarkYellow -> "DarkYellow"
    DarkBlue -> "DarkBlue"
    DarkMagenta -> "DarkMagenta"
    DarkCyan -> "DarkCyan"
    Gray -> "Gray"
    DarkGray -> "DarkGray"
    Red -> "Red"
    Green -> "Green"
    Yellow -> "Yellow"
    Blue -> "Blue"
    Magenta -> "Magenta"
    Cyan -> "Cyan"
    White -> "White"
    ColorDefault -> "ColorDefault"

fun idx/int(c: color): int
  match c
    Black -> 0
    DarkRed -> 1
    DarkGreen -> 2
    DarkYellow -> 3
    DarkBlue -> 4
    DarkMagenta -> 5
    DarkCyan -> 6
    Gray -> 7 
    DarkGray -> 8
    Red -> 9
    Green -> 10
    Yellow -> 11
    Blue -> 12
    Magenta -> 13
    Cyan -> 14
    White -> 15
    ColorDefault -> 16

fun color/cmp(a: color, b: color): order
  a.int.cmp(b.int)

//--------------------------------------------------------------------------
//  Simple monochrome printer
//--------------------------------------------------------------------------
pub val no-color-printer = 
  handler
    fun push-color-settings(s: print-color-settings) ()
    fun update-push-color-settings(f: print-color-settings -> print-color-settings) ()
    fun pop-color-settings() ()

// On windows, we cannot print unicode characters :-(
pub fun sanitize(s: string): <ndet> string
  match is-windows()
    False -> s
    True -> s.map() fn (x) {
        if (x > '~') then '?'
        else x
      }

pub val mono-printer = handler
  fun write(s) s.sanitize().print()
  fun write-ln(s) s.sanitize().println()
  fun flush() ()

//--------------------------------------------------------------------------
//  Simple File printer
//--------------------------------------------------------------------------
// Use a file printer that ignores colors. Appends, or creates the file
pub fun file-printer(file-path: string, action)
  var f := try{ read-text-file(file-path.path) }.default("")
  handle action
    fun write(s) f := f ++ s
    fun write-ln(s) f := f ++ s ++ "\n"
    fun flush() try{write-text-file(file-path.path, f)}.default(()) // TODO: Start by clearing the file, and on flush, seek to the end and append just what was added since last write

pub fun io-printer(action)
  handle action
    fun write(s) s.print()
    fun write-ln(s) s.println()
    fun flush() ()

// Use a file printer that ignores colors. Creates or overwrites the file
pub fun new-file-printer(file-path: string, action)
  var f := ""
  handle action
    fun write(s) f := f ++ s
    fun write-ln(s) f := f ++ s ++ "\n"
    fun flush() try{write-text-file(file-path.path, f)}.default(())

pub fun string-printer(action: () -> <printer|e> ()): e string
  var res := ""
  with handler
    fun write(s) res := res ++ s
    fun write-ln(s) res := res ++ s ++ "\n"
    fun flush() ()
  mask<local>{action()}
  res

// --------------------------------------------------------------------------
//  Standard ANSI escape sequences
// --------------------------------------------------------------------------
// Use a color printer that uses ANSI escape sequences.

pub fun ansi-color-settings(action)
  var settings := [PrintColorSettings()]
  with handler
    fun push-color-settings(s)
      match settings
        Cons(old, _) -> 
          ansi-escape(seq-set-console(old, s)).write()
          settings := Cons(s, settings)
        Nil -> ()
    fun update-push-color-settings(f) 
      match settings
        Cons(old, _) -> 
          val new = f(old)
          ansi-escape(seq-set-console(old, new)).write()
          settings := Cons(new, settings)
        Nil -> ()
    fun pop-color-settings() 
      match settings
        Cons(new, Cons(old, rest)) ->
          ansi-escape(seq-set-console(new, old)).write()
          settings := Cons(old, rest)
        _ -> ()
  action()

pub fun ansi-printer(action)
  with io-printer
  ansi-color-settings(action)

pub fun ansi-string-printer(action)
  with string-printer
  ansi-color-settings(action)

pub fun ansi-escape(color-settings: string)
  "\x1B[" ++ color-settings ++ "m"

pub fun seq-reset()
  "0;"

pub fun seq-underline(u: bool): string
  if (u) then "4;" else ""

pub fun seq-reverse(r: bool): string
  if (r) then "7;" else ""

pub fun seq-bold(b: bool): string
  if (b) then "1;" else ""

pub fun seq-color(back: bool, c: color): string
  if (back) then (c.ansi-color + 10).show
  else c.ansi-color.show

pub fun reset(new: print-color-settings)
  seq-reset() ++ 
  seq-reverse(new.invert) ++ seq-underline(new.underline) ++ 
  seq-color(False, new.fcolor) ++ ";" ++ seq-color(True, new.bcolor) 

pub fun diff(a: print-color-settings, b: print-color-settings)
  val r = if (a.invert != b.invert) then seq-reverse(b.invert) else ""
  val u = if (a.underline != b.underline) then seq-underline(b.underline) else ""
  val f = if (a.fcolor != b.fcolor) then seq-color(False, b.fcolor) else ""
  val back = if (a.bcolor != b.bcolor) then seq-color(True, b.bcolor) else ""
  val f-back-sep = if (f != "" && back != "") then ";" else "";
  r ++ u ++ f ++ f-back-sep ++ back

pub fun ansi-color(c: color): int
  val idx = c.int
  if (idx < 8) then idx + 30
  else if (idx < 16) then 90 + idx - 8
  else 39

pub fun (==) (a: color, b: color): bool
  a.ansi-color == b.ansi-color

pub fun (!=) (a: color, b: color): bool
  a.ansi-color != b.ansi-color

pub fun seq-set-console(old: print-color-settings, new: print-color-settings)
  if (old.invert && !new.invert) then reset(new)
  else if (old.underline && !new.underline) then reset(new)
  else diff(old, new)

val console-printer = handler
  fun write(s) s.sanitize().print()
  fun write-ln(s) s.sanitize().println()
  fun flush() ()

pub linear effect html-raw
  fun write-raw(s: string): ()

pub fun html-string-printer(action)
  var html := ""
  with handler
    fun write(s) html := html ++ s.html-escape()
    fun write-ln(s) html := html ++ s.html-escape() ++ "<br>"
    fun flush() ()
  with fun write-raw(s) html := html ++ s
  mask<local>{action()}
  html

pub fun html-color-string-printer(action)
  with html-string-printer
  with html-color-settings
  action()

// Each push opens spans nested inside the enclosing ones, and each pop
// closes exactly the spans its push opened, so the enclosing settings apply
// again. `settings` holds each level's settings and its number of open spans.
pub fun html-color-settings(action)
  var settings := [(PrintColorSettings(),0)]
  with handler
    fun push-color-settings(new)
      match settings
        Cons((old,_), _) ->
          val n = html-set-color(old, new)
          settings := Cons((new,n), settings)
        Nil -> ()
    fun update-push-color-settings(f)
      match settings
        Cons((old,_), _) ->
          val new = f(old)
          val n = html-set-color(old, new)
          settings := Cons((new,n), settings)
        Nil -> ()
    fun pop-color-settings()
      match settings
        Cons((_,n), rest) | rest.is-cons ->
          span-reset(n)
          settings := rest
        _ -> ()
  action()

pub fun span-reset(n: int)
  if n > 0 then 
    repeat("</span>", n).write-raw()
  else ()

pub fun html-set-color(old: print-color-settings, new: print-color-settings)
  diff-text-span(old.fcolor != new.fcolor, "color", new.fcolor.html-color2) + 
  diff-text-span(old.bcolor != new.bcolor, "background-color", new.bcolor.html-color2) + 
  diff-text-span(old.underline != new.underline, "text-decoration", "underline")

// VSCode sanitized spans to only allow colors with hex codes
fun html-color2(c: color): string
  match c
    ColorDefault -> "#000000" // Should never happen for VSCode
    Black -> "#000000"
    White -> "#FFFFFF"
    DarkRed -> "#8B0000"
    DarkGreen -> "#006400"
    DarkYellow -> "#8B8000"
    DarkBlue -> "#00008B"
    DarkMagenta -> "#8B008B"
    DarkCyan -> "#008B8B"
    Gray -> "#808080"
    DarkGray -> "#A9A9A9"
    Red -> "#FF0000"
    Green -> "#008000"
    Yellow -> "#FFFF00"
    Blue -> "#0000FF"
    Magenta -> "#FF00FF"
    Cyan -> "#00FFFF"

fun diff-text-span(changed: bool, prop: string, v: string)
  if changed then 
    html-text-span(prop, v)
    1
  else 0

pub fun html-text-span(prop: string, v: string)
  write-raw("<span style='" ++ prop ++ ":" ++ v ++ ";'>")

pub fun html-escape(s: string): string
  val r = s.list.map fn(c)
    match c
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      '\"' -> "&quot;"
      '\'' -> "&apos;"
      '_'  -> "&#95;"
      _ -> c.string
  r.join
