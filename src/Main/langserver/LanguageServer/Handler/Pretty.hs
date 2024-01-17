------------------------------------------------------------------------------
-- Copyright 2023, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The LSP handler that provides hover tooltips
-----------------------------------------------------------------------------

module LanguageServer.Handler.Pretty (ppComment, asKokaCode) where

import Debug.Trace
import Data.Char(isSpace)
import Lib.PPrint (Doc, text, hcat, displayS, renderCompact, (<.>), empty)
import Syntax.Colorize (removeComment)

ppComment :: String -> Doc
ppComment s
  = -- trace ("ppComment: bare comment:\n" ++ show (removeComment s)) $
    if null (filter (not . isSpace) s)
      then empty
      else hline <.> (hcat $ map fmtLine $ dropWhile null $ lines $ removeComment s)
  where
    fmtLine ""  = text "\n"
    fmtLine line
      = if last line == '\\'  -- madoko line break to markdown one
          then text (init line ++ "  \n")
          else text (line ++ "\n")


asKokaCode :: Doc -> Doc
asKokaCode doc = let code   = displayS (renderCompact doc) ""
                     wrap s = "```koka\n" ++ s ++ "\n```"
                     txt    = wrap code
                 in -- trace ("hover code:\n" ++ txt) $
                    text txt

hline = text "\n* * *\n"