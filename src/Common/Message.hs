-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Pretty print helpers for messages.
-}
-----------------------------------------------------------------------------
module Common.Message(
                     -- * Pretty print helpers
                       table, tablex, ppRange
                     -- * Source from range
                     , docFromRange, docsFromRanges
                     ) where

import Data.Char      ( isSpace )
import Lib.PPrint     
import Common.Failure (failure)
import Common.Range
import Common.ColorScheme

{--------------------------------------------------------------------------
  Pretty print helpers
--------------------------------------------------------------------------}
ppRange :: Bool -> ColorScheme -> Range -> Doc
ppRange endToo colors r
  = color (colorRange colors) (text (showRange endToo r))

table :: [(Doc,Doc)] -> Doc
table xs  = tablex 1 xs

tablex n xs
  = let (headers,docs) = unzip xs
        headerwidth    = maximum (map (length . show) headers)
    in indent n $
       if (headerwidth <= 0)
        then vcat (map snd xs)
        else vcat [fill headerwidth header <> colon <+> align doc
                  | (header,doc) <- xs]

{--------------------------------------------------------------------------
  Source from range
--------------------------------------------------------------------------}
sourceFromRanges :: [Range] -> [String]
sourceFromRanges ranges
  = [sourceFromRange range | range <- ranges]

docsFromRanges :: ColorScheme -> [Range] -> [Doc]
docsFromRanges colors ranges
  = map (docFromRange colors) ranges

docFromRange :: ColorScheme -> Range -> Doc
docFromRange colors range
  = case map (limitLineLen 55) (limitLines 3 (lines (sourceFromRange range))) of
      []  -> empty
      src -> color (colorSource colors) (align (vcat (map text src)))
  where
    limitLineLen n line
      = if (length line <= n)
         then line
         else let n2    = div n 2
                  (x,y) = splitAt n2 line
                  pre   = reverse (dropWhile (not . isSpace) (reverse x))
                  post  = dropWhile (not . isSpace) (reverse (take (n2-5) (reverse y)))
              in pre ++ " ... " ++ post

    limitLines n ls
      = if (length ls <= n)
         then removeIndent ls
        else if (n <= 2)
         then failure "Message.docFromRange.limitLines: illegal n"
         else let n2   = div n 2
                  pre  = take n2 ls
                  post = reverse (take n2 (reverse ls))
                  prepost = removeIndent (pre ++ post)
              in take n2 prepost ++ ["..."] ++ drop n2 prepost
      where
        removeIndent ls
          = let i = minimum (0:(map (length . takeWhile isSpace) ls))
            in map (drop i) ls
