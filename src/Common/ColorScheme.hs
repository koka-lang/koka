-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Global color scheme used for pretty printing
-}
-----------------------------------------------------------------------------
module Common.ColorScheme( ColorScheme(..)
                         , Color(..)
                         , defaultColorScheme
                         -- * Flags
                         , readColorFlags
                         , ansiColor
                         ) where

import Data.Char( toLower, isSpace )
import Lib.PPrint
import Lib.Printer

{--------------------------------------------------------------------------
  ColorScheme
--------------------------------------------------------------------------}
-- | Color scheme for the interpreter
data ColorScheme  = ColorScheme
                      { colorType    :: Color
                      , colorParameter   :: Color
                      , colorKind    :: Color
                      , colorMarker  :: Color
                      , colorWarning :: Color
                      , colorError   :: Color
                      , colorSource  :: Color
                      , colorInterpreter :: Color
                      , colorCommand :: Color
                      , colorKeyword :: Color
                      , colorEffect  :: Color
                      , colorRange   :: Color
                      , colorSep     :: Color
                      -- syntax coloring
                      , colorComment :: Color
                      , colorReserved:: Color
                      , colorReservedOp:: Color
                      , colorSpecial :: Color
                      , colorString  :: Color
                      , colorNumber  :: Color
                      , colorModule  :: Color
                      , colorCons    :: Color
                      , colorTypeCon :: Color
                      , colorTypeVar :: Color
                      , colorTypeKeyword :: Color
                      , colorTypeKeywordOp :: Color
                      , colorTypeSpecial :: Color
                      , colorTypeParam  :: Color
                      , colorNameQual   :: Color
                      } deriving (Show)

-- | The default color scheme
defaultColorScheme, darkColorScheme, lightColorScheme :: ColorScheme
defaultColorScheme
  = darkColorScheme

darkColorScheme  
  = let c = emptyColorScheme{ colorInterpreter = DarkRed
                            , colorCommand     = Red
                            , colorError       = Red
                            , colorComment     = DarkGreen
                            , colorReserved    = DarkYellow  
                            -- , colorReservedOp  = DarkYellow
                            , colorCons        = DarkGreen
                            , colorModule      = DarkCyan
                            , colorNameQual    = DarkGray
                            , colorString      = DarkRed
                            , colorNumber      = ColorDefault
                            , colorSource      = ColorDefault
                            , colorParameter   = DarkGray
                            , colorRange       = colorInterpreter c
                            , colorMarker      = colorInterpreter c
                            , colorWarning     = colorError c
                            , colorType        = DarkCyan -- colorSource c
                            , colorEffect      = colorType c
                            , colorTypeVar     = colorType c
                            , colorTypeCon     = colorType c
                            , colorKeyword     = colorReserved c
                            , colorTypeSpecial = colorType c
                            , colorTypeKeyword = Cyan -- colorReserved c
                            , colorTypeKeywordOp = colorType c -- colorReservedOp c
                            , colorTypeParam   = colorParameter c
                            }
    in c
  
lightColorScheme
  = let c = darkColorScheme {
                colorNumber      = DarkGray
              , colorSource      = DarkGray
              , colorCommand     = Black
              , colorInterpreter = DarkRed
              , colorError       = DarkRed
              , colorWarning     = colorError c
              , colorNameQual    = DarkGray
              , colorRange       = colorInterpreter c
              , colorMarker      = colorInterpreter c
              , colorString      = Red           
            }
    in c  

emptyColorScheme
  = makeColorScheme ColorDefault

makeColorScheme clr
  = ColorScheme clr clr clr clr clr
                clr clr clr clr
                 clr clr clr clr
                 clr clr clr clr
                 clr clr clr clr
                 clr clr clr clr
                 clr clr clr

{--------------------------------------------------------------------------
  Read colors
--------------------------------------------------------------------------}
-- | Read a comma seperated list of name=color pairs.
readColorFlags :: String -> ColorScheme -> ColorScheme
readColorFlags s scheme
  = foldl (flip readColorFlag) scheme (split s)
  where
    split :: String -> [String]
    split xs
      = let (pre,ys) = span (\c -> c /= ',' && c /= ';') xs
        in case ys of
             (',':post) -> pre : split post
             (';':post) -> pre : split post
             []         -> [pre]
             _          -> [pre,ys] -- impossible case

-- | Read a name=color flag.
readColorFlag :: String -> ColorScheme -> ColorScheme
readColorFlag s scheme
  = let (name,xs) = span (\c -> c /= '=' && c /= ':') s
    in case xs of
         (c:clr) | c=='=' || c==':' 
                   -> case (readUpdate name, readColor clr) of
                        (Just update,Just color) -> update color scheme
                        _                        -> scheme
         _         -> case readUpdate name of
                        Just update -> update ColorDefault scheme
                        _  -> scheme

readUpdate :: String -> Maybe (Color -> ColorScheme -> ColorScheme)
readUpdate s
  = lookup (norm s) updaters 


readColor :: String -> Maybe Color
readColor s
  = lookup (norm s) colors 


norm :: String -> String
norm s
  = map toLower (reverse (nowhite (reverse (nowhite s))))
  where
    nowhite s = dropWhile isSpace s

{--------------------------------------------------------------------------
  Tables
--------------------------------------------------------------------------}  
updaters  = [("type", \color scheme -> scheme{ colorType = color, colorTypeCon = color, colorTypeVar = color, colorTypeKeyword = color, colorEffect = color })
            ,("kind", \color scheme -> scheme{ colorKind = color })
            ,("marker", \color scheme -> scheme{ colorMarker = color })
            ,("warning", \color scheme -> scheme{ colorWarning = color })
            ,("error", \color scheme -> scheme{ colorError = color })
            ,("source", \color scheme -> scheme{ colorSource = color })
            ,("interpreter", \color scheme -> scheme{ colorInterpreter = color })
            ,("command", \color scheme -> scheme{ colorCommand = color })
            ,("keyword", \color scheme -> scheme{ colorKeyword = color, colorTypeKeyword = color })
            ,("typecon", \color scheme -> scheme{ colorTypeCon = color })
            ,("typevar", \color scheme -> scheme{ colorTypeVar = color })
            ,("typekeyword", \color scheme -> scheme{ colorTypeKeyword = color })
            ,("range", \color scheme -> scheme{ colorRange = color })
            ,("sep", \color scheme -> scheme{ colorSep = color })
            ,("comment", \color scheme -> scheme{ colorComment = color })
            ,("reserved", \color scheme -> scheme{ colorReserved = color })
            ,("reservedop", \color scheme -> scheme{ colorReservedOp = color })
            ,("special", \color scheme -> scheme{ colorSpecial = color })
            ,("string", \color scheme -> scheme{ colorString = color })
            ,("number", \color scheme -> scheme{ colorNumber = color })
            ,("module", \color scheme -> scheme{ colorModule = color })
            ,("effect", \color scheme -> scheme{ colorEffect = color })
            ,("parameter",\color scheme -> scheme{ colorParameter = color })
            ,("cons",\color scheme -> scheme{ colorCons = color })
            ,("constructor",\color scheme -> scheme{ colorCons = color })
            ,("none",\color scheme -> makeColorScheme color)
            ,("all",\color scheme -> makeColorScheme color)
            ]

colors  = [("black",Black)
          ,("darkred",DarkRed) 
          ,("darkgreen",DarkGreen)
          ,("darkyellow",DarkYellow)
          ,("darkblue",DarkBlue)
          ,("darkmagenta",DarkMagenta)
          ,("darkcyan",DarkCyan)
          ,("lightgray",Gray)
          ,("gray",DarkGray)
          ,("red",Red)
          ,("green",Green)
          ,("yellow",Yellow)
          ,("blue",Blue)
          ,("magenta",Magenta)
          ,("cyan",Cyan)
          ,("white",White)
          ,("default",ColorDefault)
          -- other spellings
          ,("lightgrey",Gray)
          ,("grey",DarkGray)
          ,("darkgrey",DarkGray)
          -- other words
          ,("navy",DarkBlue)
          ,("teal",DarkCyan)
          ,("maroon",DarkRed)
          ,("purple",DarkMagenta)
          ,("olive",DarkYellow)
          ,("silver",Gray)
          ,("lime",Green)
          ,("aqua",Cyan)
          ,("fuchsia",Magenta)
          ,("darkgray",DarkGray)
          ]

