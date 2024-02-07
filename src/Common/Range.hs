-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Source ranges and positions.
-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Common.Range
          ( Pos(..), makePos, minPos, maxPos
          , posMove8, posMoves8, posNull
          , Range, showFullRange
          , makeRange, rangeNull, rangeIsNull, combineRange, rangeEnd, rangeStart, rangeLength, makeSourceRange
          , Ranged( getRange ), combineRanged
          , combineRangeds, combineRanges, extendRange
          , Source(Source,sourceName, sourceBString), sourceText, sourceFromRange
          , rangeSource
          , sourceNull
          , bigLine
          , after, rangeContains, rangeIsBefore, rangeStartsAt
          , endOfRange, rangeJustBefore, rangeJustAfter
          , showRange, showCompactRange
          , BString, bstringToString, bstringToText, stringToBString
          , bstringEmpty, bstringIsEmpty
          , readInput
          , extractLiterate
          , rawSourceFromRange
          , rangeIsHidden, rangeHide
          ) where

-- import Lib.Trace
import Lib.PPrint( Pretty(pretty), text )
import Common.File(relativeToPath)
import Common.Failure( assertion, catchIO, HasCallStack )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8) -- ,decodeUtf8With)
-- import qualified Data.Text.Encoding.Error as E(lenientDecode)
-- import Common.Name(showHex)

{--------------------------------------------------------------------------
  BStrings
--------------------------------------------------------------------------}
type BString = B.ByteString

bstringIsEmpty b = B.null b

bstringEmpty = B.empty

bstringToText bstr = T.pack (BC.unpack bstr) -- utfDecode bstr -- T.decodeUtf8With E.lenientDecode bstr

bstringToString bstr
  = let s = T.unpack (T.decodeUtf8 bstr) -- (bstringToText bstr)
    in if null s then s else seq (last s) s

stringToBString str = T.encodeUtf8 (T.pack str)

readInput :: HasCallStack => FilePath -> IO BString
readInput fname
  = do input <- B.readFile fname `catchIO` (\err -> error ("unable to read " ++ fname))
       -- input <- withBinaryFile fname ReadMode $ \h -> B.hGetContents h
       -- trace ("input bytes: " ++ show (map (\c -> showHex (fromEnum c) "") (take 400 (BC.unpack input)))) $
       case BC.unpack $ B.take 3 input of
         "\xEF\xBB\xBF" -> return (B.drop 3 input) -- remove BOM
         _              -> return input


utfDecode :: B.ByteString -> T.Text
utfDecode bs
  = decode "" (BC.unpack bs)
  where
    decode acc s
      = case s of
          [] -> T.pack (reverse acc)
          (c:cs)            | (c <= '\x7F') -> decode (c:acc) cs
          (c1:c2:cs)        | (c1 >= '\xC2' && c1 <= '\xDF' && isCont c2) -> decode (decode2 c1 c2:acc) cs
          (c1:c2:c3:cs)     | (c1 >= '\xE0' && c1 <= '\xEF' && isCont c2 && isCont c3) -> decode (decode3 c1 c2 c3:acc) cs
          (c1:c2:c3:c4:cs)  | (c1 >= '\xF0' && c1 <= '\xF4' && isCont c2 && isCont c3 && isCont c4) -> decode (decode4 c1 c2 c3 c4:acc) cs

          -- modified utf8 encoding of 0
          (c1:c2:cs)        | (c1 == '\xC0' && c2 == '\x80') -> decode ('\x00':acc) cs

          -- invalid sequence of 4: use replacement
          (c1:c2:c3:c4:cs)        | (c1 >= '\xF5' && c1 <= '\xF7' && isCont c2 && isCont c3 && isCont c4) -> decode (replacement:acc) cs
          -- invalid sequence of 5: use replacement
          (c1:c2:c3:c4:c5:cs)     | (c1 >= '\xF8' && c1 <= '\xFB' && isCont c2 && isCont c3 && isCont c4 && isCont c5) -> decode (replacement:acc) cs
          -- invalid sequence of 6: use replacement
          (c1:c2:c3:c4:c5:c6:cs)  | (c1 >= '\xFC' && c1 <= '\xFD' && isCont c2 && isCont c3 && isCont c4 && isCont c5 && isCont c6) -> decode (replacement:acc) cs

          -- continuation bytes: skip
          (c:cs)            | isCont c -> decode acc cs
          -- (c1:c2:cs)        | (c1 >= '\x80' && c1 <= '\xBF' && isCont c2) -> decode (decode2 c1 c2) cs

          -- otherwise: use replacement character
          (c:cs)            -> decode (replacement:acc) cs
      where
        replacement = '\xFFFD'
        isCont c  = (c >= '\x80' && c <= '\xBF')
        decode2 :: Char -> Char -> Char
        decode2 c1 c2 = toEnum $ ((fromEnum c1 - 0xC0) * 0x40) + (fromEnum c2 - 0x80)

        decode3 :: Char -> Char -> Char -> Char
        decode3 c1 c2 c3 = toEnum $ ((fromEnum c1 - 0xE0) * 0x40 * 0x40) + ((fromEnum c2 - 0x80) * 0x40) + (fromEnum c3 - 0x80)

        decode4 :: Char -> Char -> Char -> Char -> Char
        decode4 c1 c2 c3 c4 = toEnum $ ((fromEnum c1 - 0xF0) * 0x40 * 0x40 * 0x40) + ((fromEnum c2 - 0x80) * 0x40 * 0x40) + ((fromEnum c3 - 0x80) * 0x40) + (fromEnum c4 - 0x80)


-- process literate file
extractLiterate :: BString -> BString
extractLiterate input
  = let res = B.concat (scan False input)
    in res
  where
    scan skipping input
      = if (B.null input) then [] else
        let (line,rest) = BC.span (/='\n') input
            (indent,iline) = BC.span (==' ') line
            (qs,cs) = BC.span(=='`') iline
            isQ3    = B.length qs == 3
        in if isQ3 && not skipping && (BC.all isWhite cs || startsWith cs "koka")
            then BC.pack "\n" : scanCode (BC.length indent) [] (safeTail rest)
            else BC.pack "//" : line : BC.pack "\n" :
                  scan (if (isQ3) then not skipping else skipping) (safeTail rest)

    scanCode :: Int -> [BString] -> BString -> [BString]
    scanCode indent acc input
      = if (B.null input) then [] else
        let (line,rest) = BC.span (/='\n') input
            (ind,iline) = BC.span (==' ') line
            (qs,cs) = BC.span(=='`') iline
        in if (B.length qs == 3 && BC.all isWhite cs)
            then map (\ln -> BC.snoc ln '\n') (reverse (BC.empty : acc)) ++ scan False (safeTail rest)
            else if startsWith cs "////"
             then scanCode indent (BC.empty : map (const BC.empty) acc) (safeTail rest)
            -- if (B.length qs == 1 && BC.all isWhite cs)
             -- then BC.pack "\n" : scan (safeTail rest)
             -- else
             else scanCode indent (BC.drop indent line : acc) (safeTail rest)

    safeTail bstr
      = if (B.null bstr) then B.empty else B.tail bstr

    isWhite c
      = c `elem` " \r\v\f\n"


startsWith :: BC.ByteString -> String -> Bool
startsWith bs s
  = BC.unpack (BC.take (length s) bs) == s

{--------------------------------------------------------------------------
  Source
--------------------------------------------------------------------------}
data Source = Source{ sourceName :: !FilePath, sourceBString :: !BString }
  deriving (Show)

instance Eq Source where
  (Source fname1 _) == (Source fname2 _)
    = (fname1 == fname2)

sourceNull     = Source "" B.empty
sourceText src = bstringToString (sourceBString src)

{--------------------------------------------------------------------------
  Positions
--------------------------------------------------------------------------}
-- | Source positions
data Pos    = Pos{ posSource :: !Source
                 , posOfs :: !Int
                 , posLine :: !Int
                 , posColumn :: !Int
                }

posNull = Pos sourceNull (-1) 0 0

instance Eq Pos where
  (Pos _ o1 l1 c1) == (Pos _ o2 l2 c2)
    = (l1==l2) && (c1==c2)

instance Ord Pos where
  compare (Pos _ o1 l1 c1) (Pos _ o2 l2 c2)
    = case compare l1 l2 of
        LT -> LT
        GT -> GT
        EQ -> compare c1 c2

instance Show Pos where
  show p  = "(" ++ showPos 2 p ++ ")"

showPos alignWidth (Pos src ofs line col)
  = showLine line ++ "," ++ align alignWidth (show col)

showFullPos alignWidth p
  = "[" ++ show (posOfs p) ++ "] " ++ showPos alignWidth p

-- Special showing for lines > 'bigLine'. This is used for the interpreter.
showLine line
  = if (line >= bigLine)
     then "(" ++ show (line - bigLine) ++ ")"
     else if (line <= 0)
           then "1"  -- to avoid bugs in rise4fun on 0 line numbers
           else show line

bigLine :: Int
bigLine
  = 2^26    -- about 67 million lines

align n s
  = replicate (n - length s) ' ' ++ s

-- | Create a position from a line\/column number (1-based)
makePos :: Source -> Int -> Int -> Int -> Pos
makePos = Pos


posMoves8 :: Pos -> BString -> Pos
posMoves8 pos bstr
  = BC.foldl posMove8 pos bstr

posMove8 :: Pos -> Char -> Pos
posMove8 (Pos s o l c) ch
  = let o1 = if o < 0 then o else o+1 in
    case ch of
      '\t' -> Pos s o1 l (((c+tabSize-1) `div` tabSize)*tabSize+1)
      '\n' -> Pos s o1 (l+1) 1
      _    -> Pos s o1 l (c+1)

tabSize :: Int
tabSize = 2  -- always 2 in Koka

{--------------------------------------------------------------------------
  Ranges
--------------------------------------------------------------------------}
-- | Source range
data Range  = Range{ rangeStart :: !Pos,
                     rangeEnd :: !Pos,
                     rangeIsHidden :: !Bool  -- do not add to the range map (used for internally generated expressions)
                   }
            deriving Eq

instance Show Range where
  show (Range p1 p2 _)  = show p1

instance Ord Range where
  compare (Range p1 p2 _) (Range q1 q2 _)
    = case compare p1 q1 of
        EQ   -> compare p2 q2
        ltgt -> ltgt

instance Pretty Range where
  pretty r = text (showCompactRange r)

showCompactRange :: Range -> String
showCompactRange (Range p1 p2 _)
  = "[" ++ showPos 0 p1 ++ "," ++ showPos 0 p2 ++ "]"

showRange :: FilePath -> Bool -> Range -> String
showRange cwd endToo (Range p1 p2 _)
  = (if (posLine p1 >= bigLine) then ""
      else let src = sourceName (posSource p1)
           in (relativeToPath cwd src)
    )  ++
    if (endToo)
     then ("(" ++ showPos 0 p1 ++ "-" ++ showPos 0 p2 ++ ")")
     else (show p1)



-- | Does r2 start after range r1?
after :: Range -> Range -> Bool
after r1 r2
  = rangeEnd r1 <= rangeStart r2

rangeNull :: Range
rangeNull
  = makeRange posNull posNull

rangeIsNull :: Range -> Bool
rangeIsNull (Range p1 p2 _)
  = (posOfs p1 < 0) || (posOfs p2 < 0)

showFullRange :: FilePath -> Range -> String
showFullRange cwd range
  = showRange cwd True range

-- | Make a range
makeRange :: Pos -> Pos -> Range
makeRange p1 p2
  = assertion "Range.makeRange: positions from different sources" (posSource p1 == posSource p2) $
    Range (minPos p1 p2) (maxPos p1 p2) False

makeSourceRange :: FilePath -> Int -> Int -> Int -> Int -> Range
makeSourceRange srcPath l1 c1 l2 c2
  = makeRange (makePos src (-1) l1 c1) (makePos src (-1) l2 c2)
  where
    src = Source srcPath B.empty

-- | Return the length of a range
rangeLength :: Range -> Int
rangeLength (Range p1 p2 _) = posOfs p2 - posOfs p1

-- | Return the source of a range
rangeSource :: Range -> Source
rangeSource = posSource . rangeStart

-- | Combine to ranges into a range to encompasses both.
combineRange :: Range -> Range -> Range
combineRange (Range start1 end1 h1) (Range start2 end2 h2)
  = Range (minPos start1 start2) (maxPos end1 end2) (h1 || h2)

combineRanges :: [Range] -> Range
combineRanges rs
  = foldr combineRange rangeNull rs

rangeHide :: Range -> Range
rangeHide (Range p1 p2 _)
  = Range p1 p2 True


-- | Return the minimal position
minPos :: Pos -> Pos -> Pos
minPos (Pos _ _ l _) p  | l <= 0 = p       -- for combining nullRanges sensibly
minPos p (Pos _ _ l _)  | l <= 0 = p
minPos p1 p2  = if (p1 <= p2) then p1 else p2

-- | Return the furthest position
maxPos :: Pos -> Pos -> Pos
maxPos p1 p2 = if (p1 <= p2) then p2 else p1

extendRange :: Range -> Int -> Range
extendRange (Range start end h) ofs
  = Range start (end{ posColumn = posColumn end + ofs }) h

-- | Create a range for the final character in the range
endOfRange :: Range -> Range
endOfRange range@(Range p1@(Pos _ ofs1 l1 c1) p2@(Pos src ofs2 l2 c2) h)
  = if (ofs2 - ofs1) <= 1 then range else Range p2 p2 h


rangeContains, rangeIsBefore, rangeStartsAt :: Range -> Pos -> Bool
rangeContains (Range p1 p2 _) pos
  = (p1 <= pos && p2 >= pos)

rangeIsBefore rng pos
  = rangeEnd rng < pos

rangeStartsAt rng pos
  = rangeStart rng == pos

rangeJustBefore range@(Range (Pos src ofs l c) _ h)
  = if ofs < 0 then range
     else let pos = Pos src (ofs-1) l (c-1)
          in Range pos pos h

rangeJustAfter range@(Range _ (Pos src ofs l c) h)
  = let pos = Pos src (ofs+1) l (c+1)
    in Range pos pos h

{--------------------------------------------------------------------------
  Ranged class
--------------------------------------------------------------------------}
combineRangeds :: Ranged a => [a] -> Range
combineRangeds xs
  = combineRanges (map getRange xs)

combineRanged :: (Ranged a, Ranged b) => a -> b -> Range
combineRanged x y
  = combineRange (getRange x) (getRange y)

class Ranged a where
  getRange :: a -> Range

instance Ranged Range where
  getRange r
    = r

instance Ranged r => Ranged (Maybe r) where
  getRange Nothing = rangeNull
  getRange (Just r) = getRange r




{--------------------------------------------------------------------------

--------------------------------------------------------------------------}
sourceFromRange :: Range -> String
sourceFromRange (Range start end _)  | posOfs start >= 0
  = let text = bstringToString $
               B.take (posOfs end - posOfs start) $ B.drop (posOfs start) $
               sourceBString (posSource start)
    in (replicate (posColumn start - 1) ' ' ++ text)
sourceFromRange (Range start end _)
  = case take (l2-l1+1) $ drop (l1-1) $ lines $ sourceText $ posSource start of
      (l:ls) -> case reverse ((spaces (c1-1) ++ (drop (c1-1) l)) : ls) of
                  (l:ls) -> unlines (reverse (take (c2) l : ls))
                  []     -> ""
      []     -> ""
  where
    spaces n  = take n (repeat ' ')

    c1 = posColumn start
    c2 = posColumn end
    l1 = if posLine start >= bigLine then 1 else posLine start
    l2 = if posLine end >= bigLine then (if posLine start >= bigLine then posLine end - posLine start +1 else 1) else posLine end

rawSourceFromRange :: Range -> String
rawSourceFromRange (Range start end _)
  = bstringToString $
    B.take (posOfs end - posOfs start) $ B.drop (posOfs start) $
    sourceBString (posSource start)
