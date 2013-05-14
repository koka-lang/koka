------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Instantiate type synonyms
-}
-----------------------------------------------------------------------------
module Kind.Synonym ( -- * Type synonym instantiation
                      SynInfo(..)
                    , SynonymRank, maxSynonymRank
                    --, instantiateSynonym
                    -- * Type synonyms
                    , Synonyms, synonymsNew
                    , synonymsEmpty, synonymsExtend, synonymsLookup, synonymsFind
                    , synonymsCompose
                    , synonymsIsEmpty
                    , synonymsTypeDefs
                    , synonymsDiff
                    , synonymsFilter
                    , extractSynonyms
                    , synonymsToList
                      -- * Pretty
                    , ppSynInfo, ppSynonyms
                    ) where

import qualified Data.List as L
import qualified Common.NameMap as M
import Lib.PPrint
import Common.Failure( failure )
import Common.IdNice()
import Common.Name
import Common.Syntax

import Kind.Pretty()
import Type.Type
import Type.Pretty
import Type.TypeVar()

import qualified Core.Core as Core

{--------------------------------------------------------------------------
   Synonym map
--------------------------------------------------------------------------}
-- | Synonyms: a map from type synonym names to a tuple of a synonym
-- type scheme ('SynInfo'), an integer that gives the relative /rank/ of the type synonym
newtype Synonyms  = Synonyms (M.NameMap SynInfo)

synonymsEmpty :: Synonyms
synonymsEmpty
  = Synonyms M.empty

synonymsIsEmpty :: Synonyms -> Bool
synonymsIsEmpty (Synonyms m)
  = M.null m

synonymsNew :: [SynInfo] -> Synonyms
synonymsNew synInfos
  = Synonyms (M.fromList [(synInfoName synInfo, synInfo) | synInfo <- synInfos])

synonymsTypeDefs :: Synonyms -> M.NameMap SynInfo 
synonymsTypeDefs (Synonyms m)
  = m

synonymsExtend :: SynInfo -> Synonyms -> Synonyms
synonymsExtend synInfo (Synonyms m)
  = Synonyms (M.insert (synInfoName synInfo) synInfo m)

synonymsLookup :: Name -> Synonyms -> Maybe SynInfo
synonymsLookup name (Synonyms m)
  = M.lookup name m

synonymsCompose :: Synonyms -> Synonyms -> Synonyms
synonymsCompose (Synonyms m1) (Synonyms m2)
  = Synonyms (M.union m2 m1) -- ASSUME: left-biased union

synonymsFind :: Name -> Synonyms -> SynInfo
synonymsFind name syn
  = case synonymsLookup name syn of
      Nothing -> failure ("Kind.Synonyms.synonymsFind: unknown type synonym: " ++ show name)
      Just x  -> x

synonymsDiff :: Synonyms -> Synonyms -> Synonyms
synonymsDiff (Synonyms s1) (Synonyms s2)
  = Synonyms (M.difference s1 s2)

synonymsFilter :: Name -> Synonyms -> Synonyms
synonymsFilter modName (Synonyms s)
  = Synonyms (M.filterWithKey (\name info -> qualifier name == modName) s)

synonymsToList :: Synonyms -> [SynInfo]
synonymsToList (Synonyms syns)
  = map snd $ M.toList syns

instance Show Synonyms where
  show = show . pretty

instance Pretty Synonyms where
  pretty syns
    = ppSynonyms Type.Pretty.defaultEnv syns
    
ppSynonyms showOptions (Synonyms m)
    = vcat [ppSynInfo showOptions False True sscheme Public
           | (name, sscheme) <- L.sortBy (\(n1,_) (n2,_) -> compare (show n1) (show n2)) $ M.toList m]


{--------------------------------------------------------------------------
  Synonym instantiation
--------------------------------------------------------------------------}
{-instantiateSynonym :: Kind -> [KUserType Kind] -> SynInfo -> (Scheme,[Tau],[Tau])
instantiateSynonym instkind args (SynInfo name kind params scheme rank)
  = assertion "Kind.Synonym.instantiateSynonym" (length args >= length params) $
    let (used,unused) = splitAt (length params) args
        sub  = subNew $ zip params args 
    in (sub |-> scheme, used, unused)-}

-- | Extract synonym environment from core
extractSynonyms :: Core.Core -> Synonyms
extractSynonyms core
  = Synonyms (M.unions (L.map extractTypeDefGroup (Core.coreProgTypeDefs core)))

extractTypeDefGroup (Core.TypeDefGroup tdefs)
  = M.unions (L.map extractTypeDef tdefs)

extractTypeDef :: Core.TypeDef -> M.NameMap SynInfo
extractTypeDef tdef
  = case tdef of
      Core.Synonym synInfo Core.Public 
        -> M.singleton (synInfoName synInfo) synInfo
      _ -> M.empty
