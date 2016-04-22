------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Map type names to type definition schemes.
-}
-----------------------------------------------------------------------------
module Kind.Newtypes( -- * Type newtypes
                      Newtypes, DataInfo(..)
                    , newtypesEmpty, newtypesExtend, newtypesLookup, newtypesFind
                    , newtypesNew, newtypesCompose
                    , newtypesIsEmpty
                    , newtypesTypeDefs       
                    , extractNewtypes
                      -- * Pretty
                    -- , ppNewtypes
                    ) where

import qualified Common.NameMap as M
import Common.Failure( failure )
import Common.Name
import Common.Syntax  ( Visibility(..))
import Type.Type

import Type.Pretty
import Lib.PPrint
import qualified Data.List as L

import qualified Core.Core as Core

{--------------------------------------------------------------------------
   Newtype map
--------------------------------------------------------------------------}
-- | Newtypes: a map from newtype names to newtype information
newtype Newtypes  = Newtypes (M.NameMap DataInfo)

newtypesEmpty :: Newtypes
newtypesEmpty
  = Newtypes M.empty

newtypesIsEmpty :: Newtypes -> Bool
newtypesIsEmpty (Newtypes m)
  = M.null m

newtypesNew :: [DataInfo] -> Newtypes
newtypesNew infos
  = Newtypes (M.fromList [(dataInfoName info, info) | info <- infos])

newtypesCompose :: Newtypes -> Newtypes -> Newtypes 
newtypesCompose (Newtypes m1) (Newtypes m2) 
  = Newtypes (M.union m2 m1) -- ASSUME: left-biased union

newtypesTypeDefs :: Newtypes -> M.NameMap DataInfo 
newtypesTypeDefs (Newtypes m)
  = m 

newtypesExtend :: Name -> DataInfo -> Newtypes -> Newtypes
newtypesExtend name info (Newtypes m)
  = Newtypes (M.insert name info m)

newtypesLookup :: Name -> Newtypes -> Maybe DataInfo
newtypesLookup name (Newtypes m)
  = M.lookup name m

newtypesFind :: Name -> Newtypes -> DataInfo
newtypesFind name syn
  = case newtypesLookup name syn of
      Nothing -> failure ("Kind.Newtypes.newtypesFind: unknown newtype: " ++ show name)
      Just x  -> x


-- | Extract data infos from core
extractNewtypes :: Core.Core -> Newtypes
extractNewtypes core
  = newtypesNew (concatMap extractTypeDefGroup (Core.coreProgTypeDefs core))

extractTypeDefGroup (Core.TypeDefGroup tdefs)
  = concatMap extractTypeDef tdefs

extractTypeDef :: Core.TypeDef -> [DataInfo]
extractTypeDef tdef
  = case tdef of
      Core.Data dataInfo Public conViss False
        -> [dataInfo]
      _ -> []

{--------------------------------------------------------------------------
  Pretty printing

  TODO: redo
--------------------------------------------------------------------------}

instance Show Newtypes where
  show = show . pretty

instance Pretty Newtypes where
  pretty syns
    = ppNewtypes Type.Pretty.defaultEnv syns
    
ppNewtypes showOptions (Newtypes m)
    = vcat [fill 8 (pretty name) <> colon <+>
            -- text "rank" <+> pretty rank <> colon <+>
            ppDataInfo defaultEnv True False dataInfo 
           | (name,dataInfo) <- L.sortBy (\(n1,_) (n2,_) -> compare (show n1) (show n2)) $ M.toList m]

