import std/os/path
import std/os/file
import std/os/env

fun is-windows()
  get-os-name() == "windows"

linear effect printer
  fun write(text: string): ()
  fun write-ln(text: string): ()
  fun flush(): ()

linear effect coloredPrinter
  fun withColor(c: color): ()
  fun withBackColor(c: color): ()
  fun withReverse(b: bool): ()
  fun withUnderline(b: bool): ()
  fun withColorSettings(s: printColorSettings): ()

type color
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

fun sanitize(s: string): <ndet> string
  match is-windows()
    False -> s
    True -> s.map() fn (x) {
        if (x > '~') then '?'
        else x
      }

val monoPrinter = handler
  fun write(s) s.sanitize().print()
  fun write-ln(s) s.sanitize().println()
  fun flush() ()
  

fun filePrinter(filePath: string, action)
  var f := read-text-file(filePath.path)
  handle action
    fun write(s) f := f ++ s
    fun write-ln(s) f := f ++ s ++ "\n"
    fun flush() try-default(()) fn() write-text-file(filePath.path, f)

fun newFilePrinter(filePath: string, action)
  var f := ""
  handle action
    fun write(s) f := f ++ s
    fun write-ln(s) f := f ++ s ++ "\n"
    fun flush() try-default(()) fn() write-text-file(filePath.path, f)

effect val colorSettings: printColorSettings

fun ansiPrinter(action)
  with val colorSettings = PrintColorSettings()
  handle action
    fun write(s) s.print()
    fun write-ln(s) s.println()
    fun flush() ()

struct printColorSettings
  fcolor: color = ColorDefault
  bcolor: color = ColorDefault
  invert: bool = False
  underline: bool = False

fun ansiWithConsole(console: printColorSettings, action)
  val old = colorSettings
  with finally fn()
    ansiEscape(seqSetConsole(console, old)).write()
  with override val colorSettings = console
  ansiEscape(reset(console)).write()
  action()

fun ansiWithColor(s: string, c: color)
  ansiWithConsole(colorSettings(fcolor=c))
    s.write-ln()

fun ansiEscape(colorSettings: string)
  "\x1B[" ++ colorSettings ++ "m"

fun seqReset()
  "0;"

fun seqUnderline(u: bool): string
  if (u) then "4;" else ""

fun seqReverse(r: bool): string
  if (r) then "7;" else ""

fun seqBold(b: bool): string
  if (b) then "1;" else ""

fun seqColor(back: bool, c: color): string
  if (back) then (c.ansiColor() + 10).show
  else c.ansiColor().show

fun reset(new: printColorSettings)
  seqReset() ++ 
  seqReverse(new.invert) ++ seqUnderline(new.underline) ++ 
  seqColor(False, new.fcolor) ++ ";" ++ seqColor(True, new.bcolor) 
 

fun diff(a: printColorSettings, b: printColorSettings)
  val r = if (a.invert != b.invert) then seqReverse(b.invert) else ""
  val u = if (a.underline != b.underline) then seqUnderline(b.underline) else ""
  val f = if (a.fcolor != b.fcolor) then seqColor(False, b.fcolor) else ""
  val back = if (a.bcolor != b.bcolor) then seqColor(True, b.bcolor) else ""
  val f-back-sep = if (f != "" && back != "") then ";" else "";
  r ++ u ++ f ++ f-back-sep ++ back

fun ansiColor(c: color)
  val idx = match c
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
  if (idx < 8) then idx + 30
  else if (idx < 16) then 90 + idx - 8
  else 39

fun (==) (a: color, b: color): bool
  a.ansiColor == b.ansiColor

fun (!=) (a: color, b: color): bool
  a.ansiColor != b.ansiColor

fun seqSetConsole(old: printColorSettings, new: printColorSettings)
  if (old.invert && !new.invert) then reset(new)
  else if (old.underline && !new.underline) then reset(new)
  else diff(old, new)

// type colorPrinter
  // PCon(p: consolePrinter)
  // PAnsi(p: ansiPrinter)
  // PMono(p: monoPrinter)
  // PFile(p: filePrinter)
  // PHTML(p: htmlPrinter)

val consolePrinter = handler
  fun write(s) s.sanitize().print()
  fun write-ln(s) s.sanitize().println()
  fun flush() ()

fun htmlPrinter(action)
  var html := ""
  handle action
    fun write(s) html := s.htmlEscape()
    fun write-ln(s) html := s.htmlEscape() ++ "\n"
    fun flush() html.println()

fun htmlSpan(s: string): <colorSettings> string
  // "<span style='" ++ prop ++ ":" ++ v ++ "'>" ++ s ++ "</span>"
  ""

fun htmlEscape(s: string): string
  val r = s.split("").map fn(c)
    match c
      "<" -> "&lt;"
      ">" -> "&gt;"
      "&" -> "&amp;"
      "\"" -> "&quot;"
      "\'" -> "&apos;"
      _ -> c
  r.join()

/// Test of printer features
fun main(): <console, exn, fsys, ndet> ()
  with consolePrinter
  "hi\xFF".write-ln() // Will not print unicode on Windows
  with monoPrinter
  "hi\xAA".write-ln() // Also will get rid of unicode on Windows
  with newFilePrinter("/tmp/tmp1.txt")
  "new".write()
  " file".write-ln()
  flush()
  with filePrinter("/tmp/tmp1.txt")
  "hi".write()
  " there".write-ln()
  flush()
  with ansiPrinter
  with override val colorSettings = PrintColorSettings(invert=True)
  "hello".ansiWithColor(Red)
  "world".ansiWithColor(Blue)
  with override val colorSettings = PrintColorSettings(underline=True)
  "hello".ansiWithColor(Red)
  "world".ansiWithColor(Blue)
  with override val colorSettings = PrintColorSettings()
  "hello".ansiWithColor(Red)
  "world".ansiWithColor(Blue)
  ansiWithConsole(PrintColorSettings(fcolor=Green))
    "hello ".write()
    "world!".write-ln()
  ansiWithConsole(PrintColorSettings(fcolor=DarkYellow, invert=True))
    "This is crazy, ".write()
    ansiWithConsole(PrintColorSettings(fcolor=DarkCyan, underline=True))
      "but it's ok".write-ln()




