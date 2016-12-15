------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
   Pretty print module based on Philip Wadlers /prettier printer/

   * Philip Wadler, /A prettier printer/ Draft paper,
     April 1997, revised March 1998.
     <http://cm.bell-labs.com/cm/cs/who/wadler/papers/prettier/prettier.ps>
-}
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Lib.PPrint
        ( Doc, Docs
        , Pretty(pretty,prettyList), putPretty

        , show, putDoc, hPutDoc, asString

        , (<>)
        , (<+>)
        , (</>), (<//>)
        , (<->), (<-->)

        , sep, fillSep, hsep, vsep
        , cat, fillCat, hcat, vcat
        , punctuate

        , align, hang, indent
        , fill, fillBreak

        , list, tupled, angled, semiBraces, encloseSep
        , angles, langle, rangle
        , parens, lparen, rparen
        , braces, lbrace, rbrace
        , brackets, lbracket, rbracket
        , dquotes, dquote, squotes, squote

        , comma, space, dot, backslash
        , semi, colon, equals

        , string, bool, int, integer, float, double, rational

        , softline, softbreak
        , empty, char, text, text', line, linebreak, nest, group
        , column, nesting, width

        , SimpleDoc(..)
        , renderPretty, renderCompact
        , displayS
        -- * Colors
        , Color(..)
        , color, bcolor
        , writePretty, writePrettyLn
        , writeDoc, writeDocW
        , isEmptyDoc
        ) where


import System.IO           -- (Handle,hPutStr,hPutChar,stdout,openFile,hClose)
import Lib.Printer  
import Platform.Runtime( finally )

import Data.Text.Encoding (encodeUtf8) -- ,decodeUtf8With)
import qualified Data.ByteString as B( hPutStr )
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as TL

import Data.Monoid (mempty, mappend)

infixr 5 </>,<//>,<->,<-->
infixr 6 <>,<+>

isEmptyDoc :: Doc -> Bool
isEmptyDoc doc
  = case doc of
      Empty -> True
      _     -> False

-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------
list            = encloseSep lbracket rbracket (text ", ")
tupled          = encloseSep lparen   rparen  (text ", ")
semiBraces      = encloseSep lbrace   rbrace  semi
angled          = encloseSep langle   rangle  comma

encloseSep left right sep ds
    = case ds of
        []  -> left <> right
        [d] -> left <> d <> right
        _   -> nest 2  -- align
               (hcat (zipWith (<>) (left : repeat sep) ds) <> right)


-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds


-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------
sep             = group . vsep
fillSep         = fold (</>)
hsep            = fold (<+>)
vsep            = fold (<->) . filter (not . isEmpty)

cat             = group . vcat
fillCat         = fold (<//>)
hcat            = fold (<>) 
vcat            = fold (<-->) . filter (not . isEmpty)

fold f []       = empty
fold f ds       = foldr1 f ds

x <> y          = x `beside` y
x <+> y         = x <> space <> y

Empty </> x     = x
x </> Empty     = x
x </> y         = x <> softline <> y

x <//> y        = x <> softbreak <> y

Empty <-> x     = x
x <-> Empty     = x
x <-> y         = x <> line <> y

x <--> y        = x <> linebreak <> y

softline        = group line
softbreak       = group linebreak

squotes         = enclose squote squote
dquotes         = enclose dquote dquote
braces          = enclose lbrace rbrace
parens          = enclose lparen rparen
angles          = enclose langle rangle
brackets        = enclose lbracket rbracket
enclose l r x   = l <> x <> r

lparen          = char '('
rparen          = char ')'
langle          = char '<'
rangle          = char '>'
lbrace          = char '{'
rbrace          = char '}'
lbracket        = char '['
rbracket        = char ']'

squote          = char '\''
dquote          = char '"'
semi            = char ';'
colon           = char ':'
comma           = char ','
space           = char ' '
dot             = char '.'
backslash       = char '\\'
equals          = char '='


-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"
string ""       = empty
string ('\n':s) = line <> string s
string s        = case (span (/='\n') s) of
                    (xs,ys) -> text xs <> string ys

bool :: Bool -> Doc
bool b          = text (show b)

int :: Int -> Doc
int i           = text (show i)

integer :: Integer -> Doc
integer i       = text (show i)

float :: Float -> Doc
float f         = text (show f)

double :: Double -> Doc
double d        = text (show d)

rational :: Rational -> Doc
rational r      = text (show r)


-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------
putPretty :: Pretty a => a -> IO ()
putPretty p
  = putDoc (pretty p)

class Pretty a where
  pretty        :: a -> Doc
  prettyList    :: [a] -> Doc
  prettyList    = list . map pretty

instance Pretty a => Pretty [a] where
  pretty        = prettyList

instance Pretty Doc where
  pretty        = id

instance Pretty () where
  pretty ()     = text "()"

instance Pretty Bool where
  pretty b      = bool b

instance Pretty Char where
  pretty c      = char c
  prettyList s  = string s

instance Pretty Int where
  pretty i      = int i

instance Pretty Integer where
  pretty i      = integer i

instance Pretty Float where
  pretty f      = float f

instance Pretty Double where
  pretty d      = double d


--instance Pretty Rational where
--  pretty r      = rational r

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = empty
  pretty (Just x)       = pretty x



-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------
fillBreak f x   = width x (\w ->
                  if (w > f) then nest f linebreak
                             else text' (spaces (f - w)))

fill f d        = width d (\w ->
                  if (w >= f) then empty
                              else text' (spaces (f - w)))

width d f       = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))


-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------
indent i d      = hang i (text' (spaces i) <> d)

hang i d        = align (nest i d)

align d         = column (\k ->
                  nesting (\i -> nest (k - i) d))   --nesting might be negative :-)



-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------
type Docs       = [Doc]

data Doc        = Empty
                | Char !Char             -- invariant: char is not '\n'
                | Text !String           -- invariant: text doesn't contain '\n'
                | Line !Bool            -- True <=> when undone by group, do not insert a space
                | Cat Doc Doc
                | Nest !Int Doc
                | Union Doc Doc         -- invariant: first lines of first doc longer than the first lines of the second doc
                | Column  (Int -> Doc)
                | Nesting (Int -> Doc)
                | Colored Bool Color Doc
                | ColoredEnd

data SimpleDoc  = SEmpty
                | SChar Int Char SimpleDoc
                | SText Int String SimpleDoc
                | SLine Int SimpleDoc
                | SColorOpen Bool Color SimpleDoc 
                | SColorClose SimpleDoc


isEmpty Empty   = True
isEmpty _       = False

empty           = Empty

char '\n'       = line
char c          = Char c

text ""         = Empty
text s          = Text s

text' x | null x    = Empty
        | otherwise = Text x

line            = Line False
linebreak       = Line True

beside Empty y  = y
beside x Empty  = x
beside x y      = Cat x y

nest i x        = Nest i x
column f        = Column f
nesting f       = Nesting f
group x         = Union (flatten x) x

color c doc     = Colored True c doc
bcolor c doc    = Colored False c doc


flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line break)    = if break then Empty else Text " "
flatten (Union x y)     = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten (Colored f c d) = Colored f c (flatten d)
flatten other           = other                     --Empty,Char,Text

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------
instance Eq Doc where
  d1 == d2 = let s1 = displayS (renderCompact d1) ""
                 s2 = displayS (renderCompact d2) ""
             in s1 == s2


-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data DocList   = Nil
               | Cons !Int Doc DocList


renderPrettyB :: Float -> Int -> Doc -> B.Builder
renderPrettyB rfrac w x
    = best 0 0 0 [(0, x)]
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: b = base nesting
      --         n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best b n k []      
        = mempty
      best b n k ((i,d):ds)
        = case d of
            Empty         -> best b n k ds
            Char c        -> B.singleton c    `mappend` best b n (k+1) ds
            Text s        -> B.fromString  s  `mappend` best b n (k+length s) ds
            Line _        -> B.singleton '\n' `mappend` (B.fromString (indentation i) `mappend` best b i i ds)
            Cat x y       -> best b n k ((i,x):((i,y):ds))
            Nest j x      -> let i' = i+j
                                 z  = if b == 0 then i' else b
                             in  best z n k ((i',x):ds)
            Union x y     -> nicest n k (best b n k ((i,x):ds))
                                        (best b n k ((i,y):ds))
            Column f      -> best b n k ((i,f k):ds)
            Nesting f     -> best b n k ((i,f i):ds)
            Colored f c x -> best b n k ((i,x):ds)
            ColoredEnd    -> best b n k ds

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.
      nicest n k x y = x -- TODO: reimplement this according to `renderPretty`

renderPretty :: Float -> Int -> Doc -> SimpleDoc
renderPretty rfrac w x
    = best 0 0 0 [(0, x)]
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: b = base nesting
      --         n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best b n k []           = SEmpty
      best b n k ((i,d):ds)
        = case d of
            Empty       -> best b n k ds
            Char c      -> let k' = k+1 in seq k' (SChar b c (best b n k' ds))
            Text s      -> let k' = k+(length s) in seq k' (SText b s (best b n k' ds))
            Line _      -> SLine i (best b i i ds)
            Cat x y     -> best b n k ((i,x):((i,y):ds))
            Nest j x    -> let i' = i+j in seq i' (best (if b==0 then i' else b) n k ((i',x):ds))
            Union x y   -> nicest n k (best b n k ((i,x):ds))
                                      (best b n k ((i,y):ds))

            Column f    -> best b n k ((i,f k):ds)
            Nesting f   -> best b n k ((i,f i):ds)
            Colored f c x -> SColorOpen f c (best b n k ((i,x):((i,ColoredEnd):ds)))
            ColoredEnd    -> SColorClose (best b n k ds)

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.
      nicest n k x y    | fits width x  = x
                        | otherwise     = y
                        where
                          width = min (w - k) (r - k + n)


fits w x        | w < 0         = False
fits w SEmpty                   = True
fits w (SChar i c x)            = fits (w - 1) x
fits w (SText i s x)            = fits (w - length s) x
fits w (SLine i x)              = True
fits w (SColorOpen f c x)       = fits w x
fits w (SColorClose x)          = fits w x

-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------
renderCompact :: Doc -> SimpleDoc
renderCompact x
    = scan 0 [x]
    where
      scan k []     = SEmpty
      scan k (d:ds) = case d of
                        Empty       -> scan k ds
                        Char c      -> let k' = k+1 in seq k' (SChar 0 c (scan k' ds))
                        Text s      -> let k' = k+(length s) in seq k' (SText 0 s (scan k' ds))
                        Line _      -> SLine 0 (scan 0 ds)
                        Cat x y     -> scan k (x:y:ds)
                        Nest j x    -> scan k (x:ds)
                        Union x y   -> scan k (y:ds)
                        Column f    -> scan k (f k:ds)
                        Nesting f   -> scan k (f 0:ds)
                        Colored f c x-> SColorOpen f c (scan k (x : ColoredEnd : ds))  
                        ColoredEnd   -> SColorClose (scan k ds)


asString :: Doc -> String
asString doc
  = displayS (renderCompact doc) ""

-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------
displayS :: SimpleDoc -> ShowS
displayS SEmpty             = id
displayS (SChar i c x)      = showChar c . displayS x
displayS (SText i s x)      = showString s . displayS x
displayS (SLine i x)        = showString ('\n':(replicate i ' ')) . displayS x
displayS (SColorOpen f c x) = displayS x
displayS (SColorClose x)    = displayS x

displayP :: Printer p => p -> Int -> SimpleDoc -> IO ()
displayP p w simpleDoc
   = do display 0 simpleDoc
        return ()
    where
      display k s                 = seq k (display' k s)

      display' k SEmpty            = return (k,SEmpty)
      display' k (SChar i c x)     | k+1 >= w && i+1 < w = display k (SLine (i+1) (skipSpaces (SChar i c x)))
      display' k (SChar i c x)     = do{ write p [c]; display (k+1) x }
      display' k (SText i s x)     | k+(length s) >= w && i+(length s) < w = display k (SLine (i+1) (skipSpaces (SText i s x)))
      display' k (SText i s x)     = do{ write p s; display (k+(length s)) x  }
      display' k (SLine i x)       = do{ writeLn p ""; write p (indentation i); display i x }
      display' k (SColorOpen f c x)= do{ let with = if f then withColor else withBackColor
                                       ; (kc,cont) <- with p c (display k x)
                                       ; display kc cont
                                       }
      display' k (SColorClose x)   = return (k,x)

      skipSpaces (SChar i c x)     | isSpace c = skipSpaces x
      skipSpaces (SText i s x)     | null s'   = skipSpaces x
                                   | otherwise = SText i s' x
                                   where
                                     s' = dropWhile isSpace s
      skipSpaces s                 = s
      isSpace c                    = (c==' ' || c=='\t')


-- | Display a document on a 'Printer'.
writePretty :: Printer p => p -> Doc -> IO ()
writePretty p doc
  = displayP p defaultWidth (renderPretty 0.8 defaultWidth doc)

writePrettyLn :: Printer p => p -> Doc -> IO ()
writePrettyLn p doc
  = writePretty p (doc <> linebreak)

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show Doc where
  showsPrec d doc       = displayS (renderPretty 0.5 defaultWidth doc)

putDoc :: Doc -> IO ()
putDoc doc              = hPutDoc stdout doc
                             

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc      
  = hPutDocW defaultWidth handle doc
    
hPutDocW :: Int -> Handle -> Doc -> IO ()
hPutDocW width handle doc
  = do s <- return $ encodeUtf8
                   $ TL.toStrict
                   $ B.toLazyText 
                   $ renderPrettyB 0.5 width doc
       B.hPutStr handle s

writeDoc :: FilePath -> Doc -> IO ()
writeDoc fpath doc
  = writeDocW defaultWidth fpath doc

writeDocW :: Int -> FilePath -> Doc -> IO ()
writeDocW width fpath doc
  = do h <- openFile fpath WriteMode
       hPutDocW width h doc `finally` hClose h

-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces :: Int -> String
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '

indentation 0  = ""
indentation 1  = " "
indentation 2  = "  "
indentation 3  = "   "
indentation 4  = "    "
indentation 5  = "     "
indentation 6  = "      "
indentation 7  = "       "
indentation 8  = "        "
indentation 9  = "         "
indentation 10 = "          "
indentation 11 = "           "
indentation 12 = "            "
indentation 13 = "             "
indentation 14 = "              "
indentation 15 = "               "
indentation 16 = "                "
indentation 17 = "                 "
indentation n  = spaces n
                
defaultWidth :: Int
defaultWidth = 200
             