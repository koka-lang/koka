-----------------------------------------------------------------------------
-- Copyright 2021 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Core.Borrowed( -- Borrowed parameter information
                      Borrowed
                    , borrowedNew
                    , borrowedEmpty
                    , borrowedExtend
                    , borrowedExtends
                    , borrowedLookup
                    , borrowedLookupFip
                    , borrowedExtendICore
                    , ppBorrowed

                    , extractBorrowDefs
                    , extractBorrowDef
                    , extractBorrowExternals
                    , extractBorrowed, borrowedCompose
                    ) where

import Lib.Trace
import Data.Maybe
import Common.Range
import Common.Failure
import qualified Data.List as L
import Lib.PPrint
import Common.Syntax( DefSort(..), ParamInfo(..), Fip, isNoFip )
import qualified Common.NameMap as M
import Common.Name
import Common.ColorScheme
import Core.Core
import Type.Pretty

import Lib.Trace



{--------------------------------------------------------------------------
  Initial
--------------------------------------------------------------------------}

-- | Map names to their borrowing info. If a name does not use borrowing
-- it may not be in here.
newtype Borrowed   = Borrowed (M.NameMap ([ParamInfo], Fip))

type BorrowDef = (Name, ([ParamInfo], Fip))

-- | The intial Borrowed
borrowedEmpty :: Borrowed
borrowedEmpty
  = Borrowed M.empty

borrowedNew :: [BorrowDef] -> Borrowed
borrowedNew xs
  = borrowedExtends xs borrowedEmpty

borrowedExtends :: [BorrowDef] -> Borrowed -> Borrowed
borrowedExtends xs borrowed
  = foldr borrowedExtend borrowed xs

borrowedExtend :: BorrowDef -> Borrowed -> Borrowed
borrowedExtend (name,(pinfos, fip)) (Borrowed borrowed)
  = Borrowed (M.insert name (pinfos, fip) borrowed)

borrowedLookup :: Name -> Borrowed -> Maybe [ParamInfo]
borrowedLookup name (Borrowed borrowed)
  = fmap fst (M.lookup name borrowed)

borrowedLookupFip :: Name -> Borrowed -> Maybe Fip
borrowedLookupFip name (Borrowed borrowed)
  = fmap snd (M.lookup name borrowed)

borrowedCompose :: Borrowed -> Borrowed -> Borrowed
borrowedCompose (Borrowed b1) (Borrowed b2)
  = Borrowed (M.union b1 b2)

borrowedExtendICore :: Core -> Borrowed -> Borrowed
borrowedExtendICore icore borrowed0 =
  borrowedExtends (extractBorrowDefs (coreProgDefs icore)) $
        borrowedExtends (extractBorrowExternals (coreProgExternals icore)) borrowed0

{--------------------------------------------------------------------------
  Get borrow information from Core
--------------------------------------------------------------------------}
extractBorrowed :: Core -> Borrowed
extractBorrowed core
  = borrowedExtends (extractBorrowDefs (coreProgDefs core) ++ extractBorrowExternals (coreProgExternals core)) borrowedEmpty

extractBorrowDefs ::  DefGroups -> [BorrowDef]
extractBorrowDefs dgs
  = concatMap extractDefGroup dgs

extractBorrowExternals :: Externals -> [BorrowDef]
extractBorrowExternals exs
  = mapMaybe extractExternal exs

extractExternal :: External -> Maybe BorrowDef
extractExternal ex
  = case ex of
    External name _ params _ _ fip _ _ -> Just (name, (params, fip))
    _ -> Nothing

extractDefGroup (DefRec defs)
  = mapMaybe (extractBorrowDef True) defs
extractDefGroup (DefNonRec def)
  = maybeToList (extractBorrowDef False def)

extractBorrowDef :: Bool -> Def -> Maybe BorrowDef
extractBorrowDef isRec def
  = case defSort def of
      DefFun pinfos fip | not (null pinfos && isNoFip fip) -> Just (defName def,(pinfos, fip))
      _ -> Nothing

instance Show Borrowed where
 show = show . pretty

instance Pretty Borrowed where
 pretty g
   = ppBorrowed Type.Pretty.defaultEnv g


ppBorrowed :: Env -> Borrowed -> Doc
ppBorrowed env (Borrowed borrowed)
   = vcat [text (show fip) <+> text "fun" <+> fill maxwidth (ppName env name) <+> tupled (map (text . show) pinfos)
       | (name,(pinfos, fip)) <- M.toList borrowed]
   where
     maxwidth      = 12
