-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Make "nice" names
-}
-----------------------------------------------------------------------------
module Common.IdNice
          ( Nice, niceEmpty, niceExtend, niceShow, nicePretty
          ) where

import Prelude      hiding ( lookup )
import Lib.PPrint( Doc, text )
import Common.Id
import Common.IdMap hiding ( filter, foldr )

----------------------------------------------------------------
--
----------------------------------------------------------------
-- | Map identifiers to /nice/ strings.
newtype Nice  = Nice (IdMap String)

-- | Initial map
niceEmpty :: Nice
niceEmpty
  = Nice empty

-- | Add fresh identifier substitutions give an (infinite) list of nice names.
niceExtend :: Ids -> [String] -> Nice -> Nice
niceExtend ids names (Nice map)
  = let usedNames = elems map
        niceNames = take (length ids) (filter (\n -> not (elem n usedNames)) names)
    in Nice (foldr (\(id,name) m -> insertWith (\new old -> old) id name m) map (zip ids niceNames))

-- | Show the nice version of an identifier
niceShow :: Nice -> Id -> String
niceShow (Nice map) id
  = case lookup id map of
      Just name -> name
      Nothing   -> show id

-- | Pretty print an identifier nicely.
nicePretty :: Nice -> Id -> Doc
nicePretty nice id
  = text (niceShow nice id)
