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

import Lib.PPrint (Doc, text, hcat, displayS, renderCompact, (<.>))
import Syntax.Colorize (removeComment)

ppComment :: String -> Doc
ppComment s
  = if null s
      then hline <.> text "<empty>"
      else hline <.> (hcat $ map (\ln -> text ln <.> text "  \n") $ dropWhile null $ lines $ removeComment s)

asKokaCode :: Doc -> Doc
asKokaCode doc = text ("```koka\n" ++ displayS (renderCompact doc) "" ++ "\n```")

hline = text "\n* * *\n"