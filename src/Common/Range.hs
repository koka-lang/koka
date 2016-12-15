-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Source ranges and positions.
-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Common.Range
          ( Pos, makePos, minPos, maxPos, posColumn, posLine
          , posMove8, posMoves8, posNull
          , Range, showFullRange
          , makeRange, rangeNull, combineRange, rangeEnd, rangeStart
          , Ranged( getRange ), combineRanged
          , combineRangeds, combineRanges
          , Source(Source,sourceName, sourceBString), sourceText, sourceFromRange
          , posSource
          , rangeSource
          , sourceNull
          , bigLine
          , after
          , showRange
          , BString, bstringToString, bstringToText, stringToBString
          , bstringEmpty
          , readInput
          , extractLiterate
          ) where

-- import Lib.Trace
import Common.Failure( assertion )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8) -- ,decodeUtf8With)
-- import qualified Data.Text.Encoding.Error as E(lenientDecode)
-- import Common.Name(showHex)

{--------------------------------------------------------------------------
  BStrings 
--------------------------------------------------------------------------}  
type BString = B.ByteString

bstringEmpty = B.empty

bstringToText bstr = T.pack (BC.unpack bstr) -- utfDecode bstr -- T.decodeUtf8With E.lenientDecode bstr  

bstringToString bstr = T.unpack (T.decodeUtf8 bstr) -- (bstringToText bstr)

stringToBString str = BC.pack str

readInput :: FilePath -> IO BString
readInput fname
  = do input <- B.readFile fname
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
            (qs,cs) = BC.span(=='`') $ BC.dropWhile (==' ') line 
            isQ3    = B.length qs == 3
        in if isQ3 && not skipping && (BC.all isWhite cs || startsWith cs "koka")
            then BC.pack "\n" : scanCode [] (safeTail rest)
            else BC.pack "//" : line : BC.pack "\n" : 
                  scan (if (isQ3) then not skipping else skipping) (safeTail rest)

    scanCode acc input
      = if (B.null input) then [] else
        let (line,rest) = BC.span (/='\n') input
            wline   = BC.dropWhile (==' ') line 
            (qs,cs) = BC.span(=='`') wline
        in if (B.length qs == 3 && BC.all isWhite cs) 
            then map (\ln -> BC.snoc ln '\n') (reverse (BC.empty : acc)) ++ scan False (safeTail rest)
            else if startsWith cs "////"
             then scanCode (BC.empty : map (const BC.empty) acc) (safeTail rest)
            -- if (B.length qs == 1 && BC.all isWhite cs) 
             -- then BC.pack "\n" : scan (safeTail rest)
             -- else 
             else scanCode (line : acc) (safeTail rest)

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
      '\t' -> Pos s o1 l (((c+7) `div` 8)*8+1) 
      '\n' -> Pos s o1 (l+1) 1
      _    -> Pos s o1 l (c+1)

{--------------------------------------------------------------------------
  Ranges
--------------------------------------------------------------------------}
-- | Source range
data Range  = Range !Pos !Pos
            deriving Eq

instance Show Range where
  show (Range p1 p2)  = show p1

instance Ord Range where
  compare (Range p1 p2) (Range q1 q2)
    = case compare p1 q1 of
        EQ   -> compare p2 q2
        ltgt -> ltgt



showRange endToo (Range p1 p2)
  = (if (posLine p1 >= bigLine) then "" else sourceName (posSource p1))  ++
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

showFullRange :: Range -> String
showFullRange range
  = showRange True range

-- | Make a range
makeRange :: Pos -> Pos -> Range
makeRange p1 p2
  = assertion "Range.makeRange: positions from different sources" (posSource p1 == posSource p2) $
    Range (minPos p1 p2) (maxPos p1 p2)

-- | Return the start position of a range
rangeStart :: Range -> Pos
rangeStart (Range p1 p2)  = p1

-- | Return the end position of a range
rangeEnd :: Range -> Pos
rangeEnd   (Range p1 p2)  = p2

-- | Return the source of a range
rangeSource :: Range -> Source
rangeSource = posSource . rangeStart

-- | Combine to ranges into a range to encompasses both.
combineRange :: Range -> Range -> Range
combineRange (Range start1 end1) (Range start2 end2)
  = Range (minPos start1 start2) (maxPos end1 end2)

combineRanges :: [Range] -> Range
combineRanges rs
  = foldr combineRange rangeNull rs

-- | Return the minimal position
minPos :: Pos -> Pos -> Pos
minPos (Pos _ _ l _) p  | l <= 0 = p       -- for combining nullRanges sensably
minPos p (Pos _ _ l _)  | l <= 0 = p
minPos p1 p2  = if (p1 <= p2) then p1 else p2

-- | Return the furthest position
maxPos :: Pos -> Pos -> Pos
maxPos p1 p2 = if (p1 <= p2) then p2 else p1


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
sourceFromRange (Range start end)  | posOfs start >= 0 
  = let text = bstringToString $
               B.take (posOfs end - posOfs start) $ B.drop (posOfs start) $
               sourceBString (posSource start)
    in (replicate (posColumn start - 1) ' ' ++ text)    
sourceFromRange (Range start end) 
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

