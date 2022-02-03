-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------

module Core.Inlines ( -- Inline map
                      Inlines
                    , inlinesNew
                    , inlinesEmpty
                    , inlinesExtend, inlinesExtends
                    , inlinesLookup
                    , inlinesToList
                    , inlinesMerge
                    , inlinesFilter
                    , ppInlines

                    , extractInlineDefs
                    , extractInlineDef
                    ) where

import Lib.Trace
import Data.Maybe
import Common.Range
import Common.Failure
import qualified Data.List as L
import Lib.PPrint
import qualified Common.NameMap as M
import Common.Name
import Common.NamePrim( nameBind, nameBind2 )
import Common.ColorScheme
import Core.Core
import Type.Pretty
-- import qualified Core.CoreVar as CoreVar

import Lib.Trace



{--------------------------------------------------------------------------
  Initial
--------------------------------------------------------------------------}

newtype Inlines   = Inlines (M.NameMap InlineDef)

-- | The intial Inlines
inlinesEmpty :: Inlines
inlinesEmpty
  = Inlines M.empty

inlinesNew :: [InlineDef] -> Inlines
inlinesNew xs
  = inlinesExtends xs inlinesEmpty

inlinesExtends :: [InlineDef] -> Inlines -> Inlines
inlinesExtends xs inlines
  = foldr inlinesExtend inlines xs

inlinesExtend :: InlineDef -> Inlines -> Inlines
inlinesExtend idef (Inlines inlines)
  = Inlines (M.insert (inlineName idef) idef inlines)

inlinesLookup :: Name -> Inlines -> Maybe InlineDef
inlinesLookup name (Inlines inlines)
  = M.lookup name inlines

inlinesToList :: Inlines -> [InlineDef]
inlinesToList (Inlines m) = map snd $ M.toAscList m

-- left-biased merge
inlinesMerge :: Inlines -> Inlines -> Inlines
inlinesMerge (Inlines a) (Inlines b) = Inlines $ M.union a b

inlinesFilter :: (Name -> Bool) -> Inlines -> Inlines
inlinesFilter pred (Inlines m)
  = Inlines (M.filterWithKey (\name _ -> pred name) m)

{--------------------------------------------------------------------------
  Get suitable inline definitions from Core
--------------------------------------------------------------------------}
extractInlineDefs :: Int -> DefGroups -> [InlineDef]
extractInlineDefs costMax dgs
  = concatMap (extractDefGroup costMax) dgs

extractDefGroup costMax (DefRec defs)
  = [] -- catMaybes (map (extractInlineDef costMax True) defs)
extractDefGroup costMax (DefNonRec def)
  = maybeToList (extractInlineDef costMax False def)

extractInlineDef :: Int -> Bool -> Def -> Maybe InlineDef
extractInlineDef costMax isRec def
  = let inlinable = (isInlineable costMax def)
    in -- trace ("def: " ++ show (defName def) ++ ", " ++ show (costDef def) ++ " : inline=" ++ show inlinable) $
        if not inlinable then Nothing
         else let cost = if (defName def == nameBind2 || defName def == nameBind)  -- TODO: use generic mechanism? force-inline keyword?
                          then 0 else costDef def
              in Just (InlineDef (defName def) (defExpr def) isRec (defInline def) cost (defSort def) [])

instance Show Inlines where
 show = show . pretty

instance Pretty Inlines where
 pretty g
   = ppInlines Type.Pretty.defaultEnv g


ppInlines :: Env -> Inlines -> Doc
ppInlines env (Inlines inlines)
   = vcat [fill maxwidth (ppName env name) <+> if (inlineRec idef) then text "rec" else empty
       | (name,idef) <- M.toList inlines]
   where
     maxwidth      = 12
