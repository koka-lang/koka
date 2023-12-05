------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module for portable control of colors in a console.
    Only the color of 'stdout' is influenced by these functions.
-}
-----------------------------------------------------------------------------
module Lib.Printer(
      -- * Color
      Color(..)
      -- * Printer
      , Printer( write, writeText, writeLn, writeTextLn, flush,
                  withColor, withBackColor, withReverse, withUnderline
                --  ,setColor, setBackColor, setReverse, setUnderline 
                )
      -- * Printers
    , MonoPrinter, withMonoPrinter
    , ColorPrinter(..), withColorPrinter, withNoColorPrinter, withFileNoColorPrinter, isAnsiPrinter, isConsolePrinter
    , AnsiPrinter, withAnsiPrinter
    , AnsiStringPrinter(..), HtmlTextPrinter(..)
    , withFilePrinter, withNewFilePrinter
    , withHtmlPrinter, withHtmlColorPrinter, withHtmlTextPrinter
      -- * Misc.
    , ansiWithColor
    , ansiDefault
    , ansiColor
    ) where

import Data.List( intersperse )
-- import Data.Char( toLower )
import System.IO  ( hFlush, stdout, hPutStr, hPutStrLn, openFile, IOMode(..), hClose, Handle )
import Platform.Var( Var, newVar, putVar, takeVar )
import Platform.Runtime( finally )
import Platform.Config( exeExtension )
import qualified Platform.Console as Con

import Data.Monoid (mappend, mconcat)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Debug.Trace

import System.Console.Isocline( withTerm, termWriteLn, termWrite, termFlush )

{--------------------------------------------------------------------------
  Printer
--------------------------------------------------------------------------}
-- | A printer is an abstraction for something where we can send
-- character output to.
class Printer p where
  write           :: p -> String -> IO ()
  writeText       :: p -> T.Text -> IO ()
  writeText    p t = write p (T.unpack t)
  writeLn         :: p -> String -> IO ()
  writeTextLn     :: p -> T.Text -> IO ()
  writeTextLn  p t = writeLn p (T.unpack t)
  flush           :: p -> IO ()
  withColor       :: p -> Color -> IO a -> IO a
  withBackColor   :: p -> Color -> IO a -> IO a
  withReverse     :: p -> Bool -> IO a -> IO a
  withUnderline   :: p -> Bool -> IO a -> IO a
  setColor        :: p -> Color -> IO ()
  setBackColor    :: p -> Color -> IO ()
  setReverse      :: p -> Bool -> IO ()
  setUnderline    :: p -> Bool -> IO ()

{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}
-- | Available colors on a console. Normally, background colors are
-- converted to their /dark/ variant.
data Color  = Black
            | DarkRed
            | DarkGreen
            | DarkYellow
            | DarkBlue
            | DarkMagenta
            | DarkCyan
            | Gray
            | DarkGray
            | Red
            | Green
            | Yellow
            | Blue
            | Magenta
            | Cyan
            | White
            | ColorDefault
            deriving (Show,Eq,Ord,Enum)



{--------------------------------------------------------------------------
  Simple monochrome printer
--------------------------------------------------------------------------}

-- | On windows, we cannot print unicode characters :-(
sanitize :: String -> String
sanitize s | null exeExtension  = s
sanitize s
  = map (\c -> if (c > '~') then '?' else c) s

sanitizeT :: T.Text -> T.Text
sanitizeT s | null exeExtension = s
sanitizeT s
  = T.map (\c -> if (c > '~') then '?' else c) s


-- | Monochrome console
newtype MonoPrinter  = MonoPrinter ()

-- | Use a black and white printer that ignores colors.
withMonoPrinter :: (MonoPrinter -> IO a) -> IO a
withMonoPrinter f
  = f (MonoPrinter ())

instance Printer MonoPrinter where
  write p s             = putStr $ sanitize s
  writeText p s         = T.putStr $ sanitizeT s
  writeLn p s           = putStrLn $ sanitize s
  writeTextLn p s       = T.putStrLn $ sanitizeT s
  flush p               = hFlush stdout
  withColor p c io      = io
  withBackColor p c io  = io
  withReverse p r io    = io
  withUnderline p u io  = io
  setColor p c          = return ()
  setBackColor p c      = return ()
  setReverse p r        = return ()
  setUnderline p u      = return ()


{--------------------------------------------------------------------------
  Simple file printer
--------------------------------------------------------------------------}
-- | File printer
newtype FilePrinter = FilePrinter Handle

-- | Use a file printer that ignores colors. Appends, or creates the file
withFilePrinter :: FilePath -> (FilePrinter -> IO a) -> IO a
withFilePrinter fname f
  = do h <- openFile fname AppendMode
       x <- f (FilePrinter h)
       hFlush h
       hClose h
       return x

-- | Use a file printer that ignores colors. Creates or overwrites the file
withNewFilePrinter :: FilePath -> (FilePrinter -> IO a) -> IO a
withNewFilePrinter fname f
  = do h <- openFile fname WriteMode
       x <- f (FilePrinter h)
       hFlush h
       hClose h
       return x

instance Printer FilePrinter where
  write       (FilePrinter h) s = hPutStr h s
  writeText   (FilePrinter h) s = T.hPutStr h s
  writeLn     (FilePrinter h) s  = hPutStrLn h s
  writeTextLn (FilePrinter h) s  = T.hPutStrLn h s
  flush (FilePrinter h)      = hFlush h
  withColor p c io           = io
  withBackColor p c io       = io
  withReverse p r io         = io
  withUnderline p u io       = io
  setColor p c               = return ()
  setBackColor p c           = return ()
  setReverse p r             = return ()
  setUnderline p u           = return ()




{--------------------------------------------------------------------------
  Standard ANSI escape sequences
--------------------------------------------------------------------------}
-- | Use a color printer that uses ANSI escape sequences.
withAnsiPrinter :: (AnsiPrinter -> IO a) -> IO a
withAnsiPrinter f
  = -- withTerm $
    do ansi <- newVar ansiDefault
       finally (f (Ansi ansi)) (do ansiEscapeIO seqReset
                                   hFlush stdout)

ansiDefault
  = AnsiConsole ColorDefault ColorDefault False False


-- | Standard ANSI console
newtype AnsiPrinter = Ansi (Var AnsiConsole)
data AnsiConsole = AnsiConsole{ fcolor    :: Color
                              , bcolor    :: Color
                              , invert    :: Bool
                              , underline :: Bool
                              }

instance Printer AnsiPrinter where
  write p s             = termWrite s -- putStr s
  writeText p s         = termWrite (T.unpack s) -- T.putStr s
  writeLn p s           = termWriteLn s -- putStrLn s
  writeTextLn p s       = termWriteLn (T.unpack s) -- T.putStrLn s
  flush p               = termFlush -- hFlush stdout
  withColor p c io      = ansiWithConsole p (\con -> con{ fcolor = c }) io
  withBackColor p c io  = ansiWithConsole p (\con -> con{ bcolor = c }) io
  withReverse p r io    = ansiWithConsole p (\con -> con{ invert = r }) io
  withUnderline p u io  = ansiWithConsole p (\con -> con{ underline = u }) io
  setColor p c          = unit $ ansiSetConsole p (\con -> con{ fcolor = c })
  setBackColor p c      = unit $ ansiSetConsole p (\con -> con{ bcolor = c })
  setReverse p r        = unit $ ansiSetConsole p (\con -> con{ invert = r })
  setUnderline p u      = unit $ ansiSetConsole p (\con -> con{ underline = u })


data AnsiStringPrinter = AnsiString (Var AnsiConsole) (Var String)

instance Printer AnsiStringPrinter where
  write (AnsiString c st) s             = do
    st' <- takeVar st
    putVar st (st' ++ s)
  writeText (AnsiString c st) s         = do
    st' <- takeVar st
    putVar st (st' ++ T.unpack s)
  writeLn (AnsiString c st) s           = do
    st' <- takeVar st
    putVar st (st' ++ s ++ "\n")
  writeTextLn (AnsiString c st) s  = do
    st' <- takeVar st
    putVar st (st' ++ T.unpack s ++ "\n")
  flush p               = return () -- hFlush stdout
  withColor p c io      = ansiStringWithConsole p (\con -> con{ fcolor = c }) io
  withBackColor p c io  = ansiStringWithConsole p (\con -> con{ bcolor = c }) io
  withReverse p r io    = ansiStringWithConsole p (\con -> con{ invert = r }) io
  withUnderline p u io  = ansiStringWithConsole p (\con -> con{ underline = u }) io
  setColor p c          = unit $ ansiStringSetConsole p (\con -> con{ fcolor = c })
  setBackColor p c      = unit $ ansiStringSetConsole p (\con -> con{ bcolor = c })
  setReverse p r        = unit $ ansiStringSetConsole p (\con -> con{ invert = r })
  setUnderline p u      = unit $ ansiStringSetConsole p (\con -> con{ underline = u })



ansiStringWithConsole :: AnsiStringPrinter -> (AnsiConsole -> AnsiConsole) -> IO a -> IO a
ansiStringWithConsole p f io
  = do old <- ansiStringSetConsole p f
       finally io (ansiStringSetConsole p (const old))

ansiStringSetConsole :: AnsiStringPrinter -> (AnsiConsole -> AnsiConsole) -> IO AnsiConsole
ansiStringSetConsole (AnsiString varAnsi varString) f
  = do con <- takeVar varAnsi
       let new = f con
       str <- takeVar varString
       putVar varString $ str ++ T.unpack (ansiEscape (seqSetConsole con new))
       putVar varAnsi new
       return con

-- | Helper function to put a string into a certain color
ansiWithColor :: Color -> String -> String
ansiWithColor color s
  = let con0 = ansiDefault
        con1 = con0{ fcolor = color }
        pre  = ansiEscape (seqSetConsole con0 con1)
        post = ansiEscape (seqSetConsole con1 con0)
    in T.unpack $ pre `mappend` (T.pack s `mappend` post)

-- | Enable console color code.
unit io
  = io >> return ()

-- Console code
ansiWithConsole :: AnsiPrinter -> (AnsiConsole -> AnsiConsole) -> IO a -> IO a
ansiWithConsole p f io
  = do old <- ansiSetConsole p f
       finally io (ansiSetConsole p (const old))

ansiSetConsole :: AnsiPrinter -> (AnsiConsole -> AnsiConsole) -> IO AnsiConsole
ansiSetConsole (Ansi varAnsi) f
  = do con <- takeVar varAnsi
       let new = f con
       ansiEscapeIO (seqSetConsole con new)
       putVar varAnsi new
       return con

ansiEscapeIO :: [T.Text] -> IO ()
ansiEscapeIO xs
  | null xs   = return ()
  | otherwise = termWrite (T.unpack {-T.putStr-} (ansiEscape xs))



ansiEscape :: [T.Text] -> T.Text
ansiEscape xs
  | null xs   = T.empty
  | otherwise = T.pack "\ESC[" `mappend` ((mconcat $ intersperse (T.pack ";") xs) `mappend` T.pack "m")

seqSetConsole old new
  -- reset when any attributes are disabled
  | invert old > invert new               = reset
  | underline old > underline new         = reset
  -- no attributes are disabled, we take a diff
  | otherwise = diff
  where
    reset = concat
            [seqReset
            ,seqReverse (invert new)
            ,seqUnderline (underline new)
            ,seqColor False (fcolor new)
            ,seqColor True (bcolor new)]

    diff  = concat
            [max seqReverse invert
            ,max seqUnderline underline
            ,max (seqColor False) fcolor
            ,max (seqColor True) bcolor
            ]

    max f field
      = if (field old /= field new) then f (field new) else []

seqReset :: [T.Text]
seqReset
  = [T.pack "0"]

seqUnderline :: Bool -> [T.Text]
seqUnderline u
  = if u then [T.pack "4"] else []

seqReverse rev
  = if rev then [T.pack "7"] else []

seqBold b
  = if b then [T.pack "1"] else []

seqColor :: Bool -> Color -> [T.Text]
seqColor backGround c
  = encode (ansiColor c)
  where
    encode i
      = [T.pack $ show (i + if backGround then 10 else 0)]


ansiColor :: Color -> Int
ansiColor c
  = let i = fromEnum c
    in if (i < 8) then 30 + i
        else if (i < 16) then 90 + i - 8
         else 39

{--------------------------------------------------------------------------
  Color console code
--------------------------------------------------------------------------}
-- | A color printer supports colored output 
data ColorPrinter = PCon  ConsolePrinter
                  | PAnsi AnsiPrinter
                  | PAnsiString AnsiStringPrinter
                  | PMono MonoPrinter
                  | PFile FilePrinter
                  | PHTML HtmlPrinter
                  | PHtmlText HtmlTextPrinter

-- | Use a color-enabled printer.
withColorPrinter :: (ColorPrinter -> IO b) -> IO b
withColorPrinter f
  = {- Con.withConsole $ \success ->
    if success
     then f (PCon (ConsolePrinter ()))
     else -}
    withAnsiPrinter (f . PAnsi)

withHtmlColorPrinter :: (ColorPrinter -> IO b) -> IO b
withHtmlColorPrinter f
  = withHtmlPrinter (f. PHTML)

-- | Disable the color output of a color printer. 
-- This can be useful if one wants to avoid overloading.
withNoColorPrinter :: (ColorPrinter -> IO b) -> IO b
withNoColorPrinter f
  = withMonoPrinter (\p -> f (PMono p))

-- | Disable the color output of a color printer. 
-- This can be useful if one wants to avoid overloading.
withFileNoColorPrinter :: FilePath -> (ColorPrinter -> IO b) -> IO b
withFileNoColorPrinter fname f
  = withFilePrinter fname (\p -> f (PFile p))

-- | Is this an ANSI printer?
isAnsiPrinter :: ColorPrinter -> Bool
isAnsiPrinter cp
  = case cp of
      PAnsi ansi  -> True
      PAnsiString ansi -> True
      _           -> False

isConsolePrinter :: ColorPrinter -> Bool
isConsolePrinter cp
  = case cp of
      PCon _  -> True
      _       -> False


instance Printer ColorPrinter where
  write p s             = cmap p write write write write write write write s
  writeLn p s           = cmap p writeLn writeLn writeLn writeLn writeLn writeLn writeLn s
  flush p               = cmap p flush flush flush flush flush flush flush
  withColor p c io      = cmap p withColor withColor withColor withColor withColor withColor withColor c io
  withBackColor p c io  = cmap p withBackColor withBackColor withBackColor withBackColor withBackColor withBackColor withBackColor c io
  withReverse p r io    = cmap p withReverse withReverse withReverse withReverse withReverse withReverse withReverse r io
  withUnderline p u io  = cmap p withUnderline withUnderline withUnderline withUnderline withUnderline withUnderline withUnderline u io
  setColor p c          = cmap p setColor setColor setColor setColor setColor setColor setColor c
  setBackColor p c      = cmap p setBackColor setBackColor setBackColor setBackColor setBackColor setBackColor setBackColor c
  setReverse p r        = cmap p setReverse setReverse setReverse setReverse setReverse setReverse setReverse r
  setUnderline p u      = cmap p setUnderline setUnderline setUnderline setUnderline setUnderline setUnderline setUnderline u

cmap p f g h i j k l
  = case p of
      PCon  cp -> f cp
      PAnsi ap -> g ap
      PMono mp -> h mp
      PFile fp -> i fp
      PHTML hp -> j hp
      PAnsiString as -> k as
      PHtmlText ht -> l ht


{--------------------------------------------------------------------------
  Windows console code
--------------------------------------------------------------------------}
-- | Windows console printer
newtype ConsolePrinter = ConsolePrinter ()

instance Printer ConsolePrinter where
  write p s             = putStr $ sanitize s
  writeText p s         = T.putStr $ sanitizeT s
  writeLn p s           = putStrLn $ sanitize s
  writeTextLn p s       = T.putStrLn $ sanitizeT s
  flush p               = hFlush stdout
  withColor p c io      = Con.bracketConsole (do Con.setColor c; io)
  withBackColor p c io  = Con.bracketConsole (do Con.setBackColor c; io)
  withReverse p r io    = Con.bracketConsole (do Con.setReverse r; io)
  withUnderline p u io  = Con.bracketConsole (do Con.setUnderline u; io)
  setColor p c          = Con.setColor c
  setBackColor p c      = Con.setBackColor c
  setReverse p r        = Con.setReverse r
  setUnderline p u      = Con.setUnderline u


{--------------------------------------------------------------------------
  HTML printer
--------------------------------------------------------------------------}
data HtmlPrinter = HtmlPrinter ()

withHtmlPrinter :: (HtmlPrinter -> IO a) -> IO a
withHtmlPrinter f
  = f (HtmlPrinter ())

instance Printer HtmlPrinter where
  write p s             = putStr (htmlEscape s)
  writeText p s         = write p (T.unpack s)
  writeLn p s           = putStrLn (htmlEscape s)
  writeTextLn p s       = writeLn p (T.unpack s)
  flush p               = hFlush stdout
  withColor p c io      = htmlSpan (T.pack "color") (htmlColor c) io
  withBackColor p c io  = htmlSpan (T.pack "background-color") (htmlColor c) io
  withReverse p r io    = {- no supported -} io
  withUnderline p u io  = htmlSpan (T.pack "text-decoration") (T.pack "underline") io
  setColor p c          = return ()
  setBackColor p c      = return ()
  setReverse p r        = return ()
  setUnderline p u      = return ()


{--------------------------------------------------------------------------
  HTML Text printer
--------------------------------------------------------------------------}
data HtmlTextPrinter = HtmlTextPrinter (Var T.Text)

withHtmlTextPrinter :: (HtmlTextPrinter -> IO a) -> IO a
withHtmlTextPrinter f
  = do
    stringVar <- newVar (T.pack "")
    f (HtmlTextPrinter stringVar)

addHtml :: HtmlTextPrinter -> T.Text -> IO ()
addHtml (HtmlTextPrinter stringVar) s = do
  old <- takeVar stringVar
  putVar stringVar (old <> s)

instance Printer HtmlTextPrinter where
  write p s             = addHtml p $ T.pack $ htmlEscape s
  writeText p s         = addHtml p s
  writeLn p s           = addHtml p $ T.pack $ htmlEscape (s ++ "\n")
  writeTextLn p s       = addHtml p (s <> T.pack "\n")
  flush p               = return ()
  withColor p c io      = htmlTextSpan p (T.pack "color") (htmlColor2 c) io
  withBackColor p c io  = htmlTextSpan p (T.pack "background-color") (htmlColor2 c) io
  withReverse p r io    = {- no supported -} io
  withUnderline p u io  = htmlTextSpan p (T.pack "text-decoration") (T.pack "underline") io
  setColor p c          = return ()
  setBackColor p c      = return ()
  setReverse p r        = return ()
  setUnderline p u      = return ()


htmlSpan :: T.Text -> T.Text -> IO a -> IO a
htmlSpan prop val io
  = do T.putStr $ T.pack "<span style='"
       T.putStr $ prop
       T.putStr $ T.pack ": "
       T.putStr $ val
       T.putStr $ T.pack "'>"
       x <- io
       T.putStr $ T.pack "</span>"
       return x

htmlTextSpan :: HtmlTextPrinter -> T.Text -> T.Text -> IO a -> IO a
htmlTextSpan p prop val io
  = do 
    addHtml p (T.pack "<span style='" <> prop <> T.pack ":" <> val <> T.pack ";'>")
    x <- io
    addHtml p (T.pack "</span>")
    return x
  
htmlColor :: Color -> T.Text
htmlColor c
  = case c of
      ColorDefault  -> T.pack "black"
      _             -> T.toLower (T.pack $ show c)

-- VSCode sanitizes spans to only allow colors with hex codes
htmlColor2 :: Color -> T.Text
htmlColor2 c
  = case c of
      ColorDefault  -> T.pack "#000000"
      Black -> T.pack "#000000"
      White -> T.pack "#ffffff"
      DarkRed -> T.pack "#8B0000"
      DarkGreen -> T.pack "#006400"
      DarkYellow -> T.pack "#8B8000"
      DarkBlue -> T.pack "#00008B"
      DarkMagenta -> T.pack "#8B008B"
      DarkCyan -> T.pack "#008B8B"
      Gray -> T.pack "#808080"
      DarkGray -> T.pack "#A9A9A9"
      Red -> T.pack "#FF0000"
      Green -> T.pack "#008000"
      Yellow -> T.pack "#FFFF00"
      Blue -> T.pack "#0000FF"
      Magenta -> T.pack "#FF00FF"
      Cyan -> T.pack "#00FFFF"

htmlEscape s
  = concatMap escape s
  where
    escape c
      = case c of
          '&' -> "&amp;"
          '<' -> "&lt;"
          '>' -> "&gt;"
          '"' -> "&quot;"
          '\'' -> "&apos;"
          _   -> [c]
