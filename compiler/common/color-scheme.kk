//---------------------------------------------------------------------------
// Copyright 2024, Microsoft Research, Daan Leijen. Tim Whiting
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//---------------------------------------------------------------------------
// Updated as of 7/12/25: Commit 0c7d513
module compiler/common/color-scheme
import pretty/printer
import std/core/undiv

// ----------------------------
// Global color scheme used for pretty printing
// ----------------------------

// ----------------------------
// Color Scheme
// ----------------------------

// Color scheme for the interpreter
pub effect val colors: color-scheme;

pub fun color-scheme-default(f)
  handle(f)
    val colors = dark-color-scheme

pub struct color-scheme
  colorInterpreter: color = DarkGreen
  colorType: color = DarkCyan
  colorParameter: color = DarkGray
  colorKind: color = ColorDefault
  colorMarker: color = Red
  colorError: color = Red
  colorWarning: color = DarkYellow
  colorSource: color = ColorDefault
  colorCommand: color = Green
  colorKeyword: color = DarkYellow
  colorEffect: color = colorType
  colorRange: color = DarkCyan
  colorSep: color = ColorDefault
  // syntax coloring
  colorComment: color = DarkGreen
  colorReserved: color = DarkYellow
  colorReservedOp: color = ColorDefault
  colorSpecial: color = ColorDefault
  colorString: color = Cyan
  colorNumber: color = ColorDefault 
  colorModule: color = DarkCyan
  colorCons: color = Yellow
  colorTypeCon: color = colorType
  colorTypeVar: color = colorType
  colorTypeKeyword: color = Blue
  colorTypeKeywordOp: color = colorType
  colorTypeSpecial: color = colorType
  colorTypeParam: color = colorParameter
  colorNameQual: color = DarkGray
  colorImplicitParameter: color = Gray
  colorImplicitExpr: color = colorSource

pub fun cs/(==)(c1: color-scheme, c2: color-scheme): bool
  c1.colorInterpreter == c2.colorInterpreter && c1.colorType == c2.colorType && c1.colorParameter == c2.colorParameter && c1.colorKind == c2.colorKind && c1.colorMarker == c2.colorMarker 
  && c1.colorError == c2.colorError && c1.colorWarning == c2.colorWarning && c1.colorSource == c2.colorSource && c1.colorCommand == c2.colorCommand && c1.colorKeyword == c2.colorKeyword 
  && c1.colorEffect == c2.colorEffect && c1.colorRange == c2.colorRange && c1.colorSep == c2.colorSep 
  && c1.colorComment == c2.colorComment && c1.colorReserved == c2.colorReserved && c1.colorReservedOp == c2.colorReservedOp && c1.colorSpecial == c2.colorSpecial 
  && c1.colorString == c2.colorString && c1.colorNumber == c2.colorNumber && c1.colorModule == c2.colorModule && c1.colorCons == c2.colorCons 
  && c1.colorTypeCon == c2.colorTypeCon && c1.colorTypeVar == c2.colorTypeVar && c1.colorTypeKeyword == c2.colorTypeKeyword && c1.colorTypeKeywordOp == c2.colorTypeKeywordOp 
  && c1.colorTypeSpecial == c2.colorTypeSpecial && c1.colorTypeParam == c2.colorTypeParam 
  && c1.colorNameQual == c2.colorNameQual && c1.colorImplicitParameter == c2.colorImplicitParameter && c1.colorImplicitExpr == c2.colorImplicitExpr

// The struct's field defaults are the dark theme before its remaining defaults become white.
pub val dark-color-scheme = ColorScheme().scheme/default(White)
pub val light-color-scheme = dark-color-scheme(
  colorNumber = DarkGray,
  colorSource = ColorDefault,
  colorSep = ColorDefault,
  colorSpecial = ColorDefault,
  colorCommand = Black,
  colorError = Red,
  colorWarning = DarkYellow,
  colorNameQual = DarkGray,
  colorRange = dark-color-scheme.colorInterpreter,
  colorMarker = dark-color-scheme.colorInterpreter,
  colorString = DarkRed
).scheme/default(Black)

pub val color-themes = [("dark", dark-color-scheme), ("light", light-color-scheme)]

// Replace "default" with an explicit color value
pub fun color/default(c: color, clr: color): color
  if clr == ColorDefault then c else clr

// Replace every `ColorDefault` of the scheme with `clr`
pub fun scheme/default(cs: color-scheme, clr: color): color-scheme
  cs(
    colorType = clr.color/default(cs.colorType),
    colorParameter = clr.color/default(cs.colorParameter),
    colorKind = clr.color/default(cs.colorKind),
    colorMarker = clr.color/default(cs.colorMarker),
    colorWarning = clr.color/default(cs.colorWarning),
    colorError = clr.color/default(cs.colorError),
    colorSource = clr.color/default(cs.colorSource),
    colorInterpreter = clr.color/default(cs.colorInterpreter),
    colorCommand = clr.color/default(cs.colorCommand),
    colorKeyword = clr.color/default(cs.colorKeyword),
    colorEffect = clr.color/default(cs.colorEffect),
    colorRange = clr.color/default(cs.colorRange),
    colorSep = clr.color/default(cs.colorSep),
    // syntax coloring
    colorComment = clr.color/default(cs.colorComment),
    colorReserved = clr.color/default(cs.colorReserved),
    colorReservedOp = clr.color/default(cs.colorReservedOp),
    colorSpecial = clr.color/default(cs.colorSpecial),
    colorString = clr.color/default(cs.colorString),
    colorNumber = clr.color/default(cs.colorNumber),
    colorModule = clr.color/default(cs.colorModule),
    colorCons = clr.color/default(cs.colorCons),    
    colorTypeCon = clr.color/default(cs.colorTypeCon),
    colorTypeVar = clr.color/default(cs.colorTypeVar),
    colorTypeKeyword = clr.color/default(cs.colorTypeKeyword),
    colorTypeKeywordOp = clr.color/default(cs.colorTypeKeywordOp),
    colorTypeSpecial = clr.color/default(cs.colorTypeSpecial),
    colorTypeParam = clr.color/default(cs.colorTypeParam),
    colorNameQual = clr.color/default(cs.colorNameQual),
    colorImplicitParameter = clr.color/default(cs.colorImplicitParameter),
    colorImplicitExpr = clr.color/default(cs.colorImplicitExpr)
  )

pub val empty = make-color-scheme(ColorDefault)

pub fun make-color-scheme(clr: color)
  ColorScheme(clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr,
    clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr, clr)

// ----------------------------
// Read colors
// ----------------------------
// Read a comma separated list of name=color pairs.
pub fun read-color-flags(scheme: color-scheme, s: string)
  fun split(xs: list<char>): list<string>
    val (pre, ys) = xs.span(fn(c) c != ',' && c != ';')
    match ys
      Cons(',', post) -> Cons(pre.string, post.pretend-decreasing.split)
      Cons(';', post) -> Cons(pre.string, post.pretend-decreasing.split)
      Nil -> Cons(pre.string, Nil)
      _ -> [pre.string, ""] // impossible case

  s.list.split.foldl(scheme, fn(sc, s') sc.read-color-flag(s'))

// Read a name=color flag. An unknown name or color leaves the scheme as it is.
pub fun read-color-flag(scheme: color-scheme, s: string)
  val (name, xs) = s.list.span(fn(c) c != '=' && c != ':')
  match xs
    Cons(c, clr) | c == '=' || c == ':' ->
      match (read-update(name.string), read-color(clr.string))
        (Just(update), Just(color)) -> update(color, scheme)
        _ -> scheme
    _ ->
      match color-themes.find(fn((n,_)) n == name.string)
        Just((_, theme)) -> theme
        _ -> match read-update(name.string)
          Just(update) -> update(ColorDefault, scheme)
          Nothing -> scheme

fun norm(s: string): string
  s.trim.to-lower

fun read-color(s: string): maybe<color>
  match s.norm
    "black" -> Just(Black)
    "darkred" -> Just(DarkRed)
    "darkgreen" -> Just(DarkGreen)
    "darkyellow" -> Just(DarkYellow)
    "darkblue" -> Just(DarkBlue)
    "darkmagenta" -> Just(DarkMagenta)
    "darkcyan" -> Just(DarkCyan)
    "lightgray" -> Just(Gray)
    "gray" -> Just(DarkGray)
    "red" -> Just(Red)
    "green" -> Just(Green)
    "yellow" -> Just(Yellow)
    "blue" -> Just(Blue)
    "magenta" -> Just(Magenta)
    "cyan" -> Just(Cyan)
    "white" -> Just(White)
    "default" -> Just(ColorDefault)
    // other spellings
    "lightgrey" -> Just(Gray)
    "grey" -> Just(DarkGray)
    "darkgrey" -> Just(DarkGray)
    // other words
    "navy" -> Just(DarkBlue)
    "teal" -> Just(DarkCyan)
    "maroon" -> Just(DarkRed)
    "purple" -> Just(DarkMagenta)
    "olive" -> Just(DarkYellow)
    "silver" -> Just(Gray)
    "lime" -> Just(Green)
    "aqua" -> Just(Cyan)
    "fuchsia" -> Just(Magenta)
    "darkgray" -> Just(DarkGray)
    _ -> Nothing

fun read-update(name: string): maybe<(color, color-scheme) -> color-scheme>
  match name.norm
    "type" -> Just(fn(clr, scheme) scheme(colorType=clr, colorTypeCon=clr, colorTypeVar=clr, colorTypeKeyword=clr, colorEffect=clr))
    "kind" -> Just(fn(clr, scheme) scheme(colorKind=clr))
    "marker" -> Just(fn(clr, scheme) scheme(colorMarker=clr))
    "warning" -> Just(fn(clr, scheme) scheme(colorWarning=clr))
    "error" -> Just(fn(clr, scheme) scheme(colorError=clr))
    "source" -> Just(fn(clr, scheme) scheme(colorSource=clr))
    "interpreter" -> Just(fn(clr, scheme) scheme(colorInterpreter=clr))
    "command" -> Just(fn(clr, scheme) scheme(colorCommand=clr))
    "keyword" -> Just(fn(clr, scheme) scheme(colorKeyword=clr, colorTypeKeyword=clr))
    "typecon" -> Just(fn(clr, scheme) scheme(colorTypeCon=clr))
    "typevar" -> Just(fn(clr, scheme) scheme(colorTypeVar=clr))
    "typekeyword" -> Just(fn(clr, scheme) scheme(colorTypeKeyword=clr))
    "range" -> Just(fn(clr, scheme) scheme(colorRange=clr))
    "sep" -> Just(fn(clr, scheme) scheme(colorSep=clr))
    "comment" -> Just(fn(clr, scheme) scheme(colorComment=clr))
    "reserved" -> Just(fn(clr, scheme) scheme(colorReserved=clr))
    "reservedop" -> Just(fn(clr, scheme) scheme(colorReservedOp=clr))
    "special" -> Just(fn(clr, scheme) scheme(colorSpecial=clr))
    "string" -> Just(fn(clr, scheme) scheme(colorString=clr))
    "number" -> Just(fn(clr, scheme) scheme(colorNumber=clr))
    "module" -> Just(fn(clr, scheme) scheme(colorModule=clr))
    "effect" -> Just(fn(clr, scheme) scheme(colorEffect=clr))
    "parameter" -> Just(fn(clr, scheme) scheme(colorParameter=clr))
    "cons" -> Just(fn(clr, scheme) scheme(colorCons=clr))
    "constructor" -> Just(fn(clr, scheme) scheme(colorCons=clr))
    "none" -> Just(fn(clr, _) make-color-scheme(clr))
    "all" -> Just(fn(clr, _) make-color-scheme(clr))
    "implicitparameter" -> Just(fn(clr, scheme) scheme(colorImplicitParameter=clr))
    "implicitexpr" -> Just(fn(clr, scheme) scheme(colorImplicitExpr=clr))
    _ -> Nothing
